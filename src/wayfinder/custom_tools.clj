(ns wayfinder.custom-tools
  "Loads, validates, watches, and dispatches user-defined tool manifests.

  A manifest is a single top-level Clojure map in a .clj file:

    {:name \"web\"
     :description \"Search or fetch a URL\"
     :args {:mode {:type :string
                   :enum [\"search\" \"fetch\"]
                   :default \"search\"}
            :query {:type :string :required true}}
     :exec (fn [{:keys [mode query]}]
             (case mode
               \"search\" (str \"searched for \" query)
               \"fetch\" (slurp (str \"https://example.com/?q=\" query))))}

  Every value except :exec is data. :exec is Clojure code, evaluated once per
  reload in a dedicated namespace, and must yield a function of one map
  argument. The tool's :args are passed as keyword keys with :default applied
  before :exec runs."
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [cheshire.core :as json]
            [wayfinder.tools :as tools])
  (:import [java.io PushbackReader StringReader]))

(def ^:private builtin-names
  (set (map #(get-in % [:function :name]) tools/tool-definitions)))

(defonce state
  (atom {:dir nil
         :fingerprint nil
         :definitions []
         :registry {}
         :errors []
         :surfaced #{}}))

(defn- tools-dir [cfg]
  (or (:tools-dir cfg)
      (str (or (:home-dir cfg) "/home/wayfinder") "/tools")))

(defn- ensure-dir [dir]
  (let [f (io/file dir)]
    (when-not (.exists f)
      (when-not (.mkdirs f)
        (throw (ex-info (str "Could not create tools directory: " dir) {:phase :io}))))
    (when-not (.isDirectory f)
      (throw (ex-info (str "Tools path is not a directory: " dir) {:phase :io})))
    dir))

(defn- eval-namespace []
  (or (find-ns 'wayfinder.custom-tools.exec)
      (let [ns (create-ns 'wayfinder.custom-tools.exec)]
        (binding [*ns* ns]
          (clojure.core/refer-clojure)
          (clojure.core/alias 'str 'clojure.string)
          (clojure.core/alias 'io 'clojure.java.io)
          (clojure.core/alias 'shell 'clojure.java.shell)
          (clojure.core/alias 'json 'cheshire.core))
        ns)))

(defn- read-manifest-form [s]
  (binding [*read-eval* false]
    (with-open [rdr (PushbackReader. (StringReader. s))]
      (let [form (clojure.core/read {:eof ::eof} rdr)]
        (when (= ::eof form)
          (throw (ex-info "Manifest is empty" {:phase :parse})))
        (let [extra (clojure.core/read {:eof ::eof} rdr)]
          (when-not (= ::eof extra)
            (throw (ex-info "Manifest must contain exactly one top-level form" {:phase :parse}))))
        form))))

(def ^:private allowed-types
  #{"string" "integer" "number" "boolean" "array" "object"})

(defn- key-str [k]
  (if (keyword? k) (name k) (str k)))

(defn- arg-type-name [t]
  (cond
    (keyword? t) (name t)
    (string? t) t
    :else nil))

(defn- validate-arg [k spec]
  (if-not (map? spec)
    [(format "arg '%s' spec must be a map" (key-str k))]
    (cond-> []
      (not (contains? allowed-types (arg-type-name (:type spec))))
      (conj (format "arg '%s' has invalid :type %s" (key-str k) (pr-str (:type spec))))
      (and (contains? spec :enum) (not (vector? (:enum spec))))
      (conj (format "arg '%s' :enum must be a vector" (key-str k)))
      (and (contains? spec :required) (not (boolean? (:required spec))))
      (conj (format "arg '%s' :required must be boolean" (key-str k)))
      (and (contains? spec :description) (not (string? (:description spec))))
      (conj (format "arg '%s' :description must be a string" (key-str k))))))

(defn- normalize-args [args]
  (into {} (map (fn [[k v]] [(if (keyword? k) k (keyword (str k))) v])) args))

(defn- validate [m]
  (or
    (when-not (string? (:name m))
      ":name must be a string")
    (let [n (:name m)]
      (when (or (str/blank? n) (not (re-matches #"[A-Za-z0-9_-]+" n)))
        (format ":name must match [A-Za-z0-9_-]+ (got %s)" (pr-str n))))
    (when (> (count (:name m)) 64)
      ":name must be 64 characters or fewer")
    (when (contains? builtin-names (:name m))
      (format ":name '%s' shadows a built-in tool" (:name m)))
    (when-not (string? (:description m))
      ":description must be a string")
    (when (str/blank? (:description m))
      ":description must not be empty")
    (when-not (map? (:args m))
      ":args must be a map")
    (let [errs (mapcat (fn [[k spec]] (validate-arg k spec)) (:args m))]
      (when (seq errs) (str/join "; " errs)))
    (when-not (fn? (:exec m))
      ":exec must be a (fn [args] ...) form")))

(defn- defaults-map [args]
  (into {}
    (for [[k spec] args
          :when (and (map? spec) (contains? spec :default))]
      [k (:default spec)])))

(defn- required-keys [args]
  (->> args
       (filter (fn [[_ spec]]
                 (and (map? spec)
                      (true? (:required spec))
                      (not (contains? spec :default)))))
       (map key)
       (vec)))

(defn- arg-property [spec]
  (cond-> {:type (arg-type-name (:type spec))}
    (contains? spec :description) (assoc :description (:description spec))
    (contains? spec :enum) (assoc :enum (:enum spec))))

(defn- manifest->definition [{:keys [name description args]}]
  {:type "function"
   :function
   {:name name
    :description description
    :parameters
    {:type "object"
     :properties (into {} (map (fn [[k spec]] [(key-str k) (arg-property spec)])) args)
     :required (->> args
                    (filter (fn [[_ spec]] (true? (:required spec))))
                    (map (fn [[k _]] (key-str k)))
                    (vec))}}})

(defn- throwable-message [^Throwable t]
  (let [m (.getMessage t)
        c (some-> t .getCause .getMessage)]
    (if (and c (not= c m))
      (str m " — " c)
      (or m (str t)))))

(defn- load-manifest [f]
  (try
    (let [form (read-manifest-form (slurp f))]
      (when-not (map? form)
        (throw (ex-info "Top-level form must be a map" {:phase :shape})))
      (let [m (binding [*ns* (eval-namespace)] (eval form))]
        (when-not (map? m)
          (throw (ex-info "Manifest must evaluate to a map" {:phase :eval})))
        (let [m (if (map? (:args m)) (update m :args normalize-args) m)
              err (validate m)]
          (when err
            (throw (ex-info err {:phase :validation})))
          {:ok {:file (.getName f)
                :name (keyword (:name m))
                :name-str (:name m)
                :defaults (defaults-map (:args m))
                :required (required-keys (:args m))
                :definition (manifest->definition m)
                :exec (:exec m)}})))
    (catch Throwable t
      {:error {:file (.getName f)
               :phase (or (:phase (ex-data t)) :eval)
               :message (throwable-message t)
               :detail (dissoc (ex-data t) :phase)}})))

(defn- manifest-files [dir]
  (let [f (io/file dir)]
    (when (.isDirectory f)
      (->> (.listFiles f)
           (filter #(.isFile %))
           (filter #(str/ends-with? (.getName %) ".clj"))
           (sort-by #(.getName %))))))

(defn- fingerprint [dir]
  (into {}
    (map (fn [f] [(.getName f) [(.lastModified f) (.length f)]]))
    (manifest-files dir)))

(defn- dedupe-tools [oks]
  (loop [seen #{} kept [] errs [] items oks]
    (if-let [{:keys [name name-str file] :as item} (first items)]
      (if (contains? seen name)
        (recur seen kept
               (conj errs {:file file
                           :phase :validation
                           :message (format "Duplicate tool name '%s'" name-str)})
               (rest items))
        (recur (conj seen name) (conj kept item) errs (rest items)))
      {:oks kept :errors errs})))

(defn- load-manifests [dir]
  (let [results (map load-manifest (manifest-files dir))
        oks (keep :ok results)
        errs (vec (keep :error results))
        deduped (dedupe-tools oks)]
    {:definitions (mapv :definition (:oks deduped))
     :registry (into {}
                 (map (fn [{:keys [name defaults required exec]}]
                        [name {:exec exec :defaults defaults :required required}]))
                 (:oks deduped))
     :errors (into errs (:errors deduped))}))

(defn format-error [err]
  (let [{:keys [file phase message detail]} err]
    (str "custom tool manifest error"
         " — file: " (or file "<unknown>")
         ", phase: " (name (or phase :unknown))
         "\n  " (or message "(no message)")
         (when (seq detail)
           (str "\n  detail: " (pr-str detail))))))

(defn- reload! [dir]
  (let [{:keys [definitions registry errors]} (load-manifests dir)]
    (swap! state assoc
           :dir dir
           :fingerprint (fingerprint dir)
           :definitions definitions
           :registry registry
           :errors errors
           :surfaced #{})
    (doseq [e errors]
      (println (format "[custom-tools] ERROR: %s" (format-error e))))
    (println (format "[custom-tools] Loaded %d tool(s) from %s" (count definitions) dir))))

(defn- watch-loop [dir interval-ms stop?]
  (loop []
    (when-not @stop?
      (try
        (let [fp (fingerprint dir)]
          (when (not= fp (:fingerprint @state))
            (println (format "[custom-tools] Change detected in %s — reloading" dir))
            (reload! dir)))
        (catch Throwable t
          (println (format "[custom-tools] Watcher error: %s" (throwable-message t))))))
    (Thread/sleep (long interval-ms))
    (recur)))

(defn start!
  "Ensure the tools directory exists, load all manifests, and start a watcher
   that reloads on change. Never throws; startup failure is recorded as an
   error and surfaced to the agent. Returns a stop fn."
  [cfg]
  (let [dir (tools-dir cfg)]
    (try
      (ensure-dir dir)
      (reload! dir)
      (catch Throwable t
        (println (format "[custom-tools] Startup failed: %s" (throwable-message t)))
        (swap! state assoc
               :dir dir
               :fingerprint nil
               :definitions []
               :registry {}
               :errors [{:file "<tools-dir>"
                         :phase :io
                         :message (throwable-message t)}]
               :surfaced #{})))
    (let [stop? (atom false)]
      (future (watch-loop dir (long (or (:tools-watch-ms cfg) 1000)) stop?))
      #(reset! stop? true))))

(defn definitions
  "Built-in tool definitions plus the current custom tool definitions."
  []
  (into tools/tool-definitions (:definitions @state)))

(defn exec-for [name]
  (when name
    (get-in @state [:registry (keyword name)])))

(defn- stack-trace-str [^Throwable t]
  (with-out-str
    (doseq [e (take 8 (.getStackTrace t))]
      (println "  at" (str e)))))

(defn invoke
  "Run a custom tool by name with the parsed :params map. Returns
   {:content ...}. Execution errors and missing required arguments become
   loud, structured content so the agent can fix the manifest."
  [name params]
  (if-let [{:keys [exec defaults required]} (exec-for name)]
    (try
      (let [args (merge defaults params)
            missing (seq (remove #(contains? args %) required))]
        (when missing
          (throw (ex-info (format "Missing required argument(s): %s"
                            (str/join ", " (map key-str missing)))
                          {:phase :invoke})))
        (let [result (exec args)]
          (cond
            (string? result) {:content result}
            (and (map? result) (contains? result :content)) result
            (nil? result) {:content "(tool returned nil)"}
            :else {:content (pr-str result)})))
      (catch Throwable t
        {:content (format "Custom tool '%s' failed (phase %s): %s\n%s"
                    (key-str name)
                    (name (or (:phase (ex-data t)) :runtime))
                    (or (.getMessage t) (str t))
                    (stack-trace-str t))}))
    {:content (format "Unknown custom tool: %s" (key-str name))}))

(defn new-errors
  "Return tool-manifest errors that have not yet been surfaced to the agent,
   and mark them surfaced. The set resets on every reload, so a change to any
   manifest re-reports every still-broken manifest exactly once."
  []
  (let [{:keys [errors surfaced]} @state
        unseen (filterv #(not (contains? surfaced [(:file %) (:phase %) (:message %)])) errors)]
    (swap! state update :surfaced into (map (fn [e] [(:file e) (:phase e) (:message e)]) unseen))
    unseen))
