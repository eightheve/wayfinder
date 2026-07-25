(ns wayfinder.agent
  (:require [wayfinder.context :as context]
            [wayfinder.prompt :as prompt]
            [wayfinder.llm :as llm]
            [wayfinder.tools :as tools]
            [wayfinder.dispatch :as dispatch]
            [wayfinder.compactor :as compactor]
            [wayfinder.scribe :as scribe]
            [wayfinder.matrix :as matrix]
            [cheshire.core :as json]
            [clojure.edn]
            [clojure.pprint]
            [clojure.set]
            [clojure.string])
  (:import [java.io File]))

(def default-delay 5000)

(defn load-system-prompt [dir]
  (let [files (->> (file-seq (File. dir))
                   (filter #(.isFile %))
                   (filter #(.endsWith (.getName %) ".md"))
                   (sort-by #(.getName %)))]
    (->> (map slurp files)
         (clojure.string/join "\n\n"))))

(defn parse-tool-calls [response]
  (when-let [calls (:tool_calls response)]
    (for [call calls]
      (let [func (:function call)]
        {:action-type (keyword (:name func))
         :params (try (json/parse-string (:arguments func) true)
                      (catch Exception _ {}))
         :call-id (:id call)}))))

(defn- context-file [cfg]
  (str (or (:state-dir cfg) "/var/lib/wayfinder") "/context.edn"))

(defn save-context
  "Persist live context after every turn (atomic write) so a restart
   resurrects the session instead of rebooting an amnesiac."
  [ctx cfg]
  (try
    (let [f (java.io.File. (context-file cfg))
          tmp (java.io.File. (str (context-file cfg) ".tmp"))]
      (.mkdirs (.getParentFile f))
      (spit tmp (pr-str @ctx))
      (.renameTo tmp f))
    (catch Exception e
      (println (format "[agent] save-context failed: %s" (.getMessage e))))))

(defn load-context [cfg]
  (try
    (let [f (java.io.File. (context-file cfg))]
      (when (.exists f)
        (let [state (clojure.edn/read-string (slurp f))]
          (if (and (map? state) (vector? (:items state)) (int? (:next-id state)))
            ;; A context.edn written before the ledger existed resurrects with
            ;; an empty done-list rather than nil — a reset costs the history,
            ;; never the mechanism.
            (let [state (update state :ledger #(if (vector? %) % []))]
              (println (format "[agent] Resurrected context: %d items, next-id %d, %d ledger entries"
                         (count (:items state)) (:next-id state) (count (:ledger state))))
              state)
            (do (println "[agent] context.edn malformed — starting fresh")
                nil)))))
    (catch Exception e
      (println (format "[agent] load-context failed (%s) — starting fresh" (.getMessage e)))
      nil)))

(defn dump-context [ctx cfg]
  (try
    (let [dir (str (or (:state-dir cfg) "/var/lib/wayfinder") "/debug")]
      (.mkdirs (java.io.File. dir))
      (spit (str dir "/context") (with-out-str (clojure.pprint/pprint @ctx))))
    (catch Exception e
      (println (format "[agent] dump-context failed: %s" (.getMessage e))))))

(defn call-llm [ctx cfg system-prompt idle-count]
  (let [messages (prompt/assemble @ctx system-prompt idle-count)
        base-url (:base-url cfg)
        api-key (:api-key cfg)
        agent-cfg (get-in cfg [:agents :main])
        model (:model agent-cfg)
        effort (:reasoning-effort agent-cfg)]
    (println (format "[agent] Calling LLM (%d items in context)" (count (:items @ctx))))
    (llm/complete base-url api-key model messages tools/tool-definitions effort)))

(def ^:private max-result-length 10000)

(defn- token-set [s]
  (set (remove empty? (clojure.string/split (clojure.string/lower-case (str s)) #"[^\p{L}\p{N}]+"))))

(defn- similarity
  "Word-set Jaccard similarity, 0.0-1.0."
  [a b]
  (let [ta (token-set a) tb (token-set b)]
    (if (or (empty? ta) (empty? tb))
      0.0
      (/ (double (count (clojure.set/intersection ta tb)))
         (count (clojure.set/union ta tb))))))

(def ^:private resend-similarity-threshold 0.6)

(defn- trunc-result [c]
  (let [c (str c)]
    (if (> (count c) max-result-length)
      (str (subs c 0 max-result-length) "...")
      c)))

(defn- ledger-opts [cfg]
  {:cap (or (:ledger-cap cfg) 30)
   :arg-length (or (:ledger-arg-length cfg) 72)})

(defn- result-ok?
  "Coarse success signal for the ledger: every failure path in this file
   reports itself by prefixing the result content."
  [content]
  (not (re-find #"(?i)^(error|access denied|move failed|delivery failed|send rejected|file not found)"
         (str content))))

(defn- recent-result-contents [ctx n]
  (->> (:items @ctx)
       (filter #(= :action-result (:type %)))
       (take-last n)
       (map #(get-in % [:data :content]))))

(defn execute-and-record [ctx cfg action recently-sent]
  (let [{:keys [action-type params call-id]} action]
    (cond
      (= action-type :reason)
      (do (swap! ctx context/add-item :reasoning {:content (:thought params)})
          nil)

      (= action-type :wait)
      (let [secs (max 5 (min 300 (:seconds params)))]
        (println (format "[agent] WAIT %ds" secs))
        ;; A long wait is a decision, not idleness: attentive waiting holds
        ;; the idle counter instead of escalating it.
        {:delay (* secs 1000) :deliberate-wait? (>= secs 60)})

      (= action-type :send-message)
      (let [content (:content params)
            _ (swap! ctx context/add-item :action {:action-type :send-message :params params :call-id call-id})
            action-id (dec (:next-id @ctx))
            near-dup (some #(when (> (similarity content %) resend-similarity-threshold) %)
                       @recently-sent)]
        (if near-dup
          ;; Hard backstop against restatement sprees: recently-sent finally
          ;; earns its keep. The rejection is reported as the tool result so
          ;; the model learns why nothing was delivered.
          (do
            (println (format "[agent] send-message REJECTED as near-duplicate (item %d, similarity > %.1f)"
                       action-id resend-similarity-threshold))
            (swap! ctx context/add-item :action-result
              {:caused-by action-id
               :content "Send REJECTED: nearly identical to a message you already sent. The user already has that message — say something genuinely new, or stay silent. Do not respond to this rejection message, it is a purely internal result."})
            ;; Ledger records attempts too: a rejected send did happen, and
            ;; seeing it listed as FAILED is how the pattern becomes visible.
            (swap! ctx context/record-action :send-message params false (ledger-opts cfg)))
          (let [_ (println (format "[agent] EXEC send-message (item %d)" action-id))
                {:keys [ok? status]} (matrix/send-message cfg content)]
            ;; Send-message is bookkept like every other tool: the action
            ;; renders as a tool_call and gets an explicit result, so the
            ;; model has first-class evidence that it spoke (or failed to).
            (swap! ctx context/add-item :action-result
              {:caused-by action-id
               :content (if ok?
                          "Message delivered."
                          (format "Delivery FAILED (status %s) — the user did NOT receive this message." status))})
            (swap! ctx context/record-action :send-message params ok? (ledger-opts cfg))
            (swap! recently-sent conj content)
            (swap! recently-sent #(vec (take-last 10 %)))))
        nil)

      :else
      (let [_ (swap! ctx context/add-item :action
                  {:action-type action-type :params params :call-id call-id})
            action-id (dec (:next-id @ctx))
            _ (println (format "[agent] EXEC %s (item %d)" (name action-type) action-id))
            result (cond
                     (= action-type :recall)
                     (do (scribe/recall ctx cfg (:query params))
                         {:content "Memory recall initiated"})

                     (= action-type :append-memory)
                     (do (println (format "[agent] APPEND-MEMORY %s" (:filename params)))
                         {:content (try
                                     (str "Appended to "
                                       (scribe/append-note cfg (:filename params) (:content params)))
                                     (catch Exception e
                                       (str "Error appending: " (.getMessage e))))})

                     (= action-type :move-memory)
                     (do (println (format "[agent] MOVE-MEMORY %s -> %s" (:from params) (:to params)))
                         {:content (try
                                     (let [{:keys [ok?]} (scribe/move-note cfg (:from params) (:to params))]
                                       (if ok?
                                         (format "Moved %s to %s" (:from params) (:to params))
                                         (format "Move failed: %s not found" (:from params))))
                                     (catch Exception e
                                       (str "Error moving: " (.getMessage e))))})

                     (= action-type :remember)
                     (do (println (format "[agent] REMEMBER %s" (:filename params)))
                         {:content (try
                                     (str "Memory written: "
                                       (scribe/remember-note cfg (:filename params) (:content params)))
                                     (catch Exception e
                                       (str "Error writing memory: " (.getMessage e))))})

                     (= action-type :list-memories)
                     (do (println "[agent] LIST-MEMORIES")
                         {:content (try (scribe/list-memories cfg)
                                        (catch Exception e
                                          (str "Error listing memories: " (.getMessage e))))})

                     (= action-type :pin-item)
                     (let [id (:id params)]
                       (swap! ctx context/update-item id {:pinned true})
                       (println (format "[agent] PIN item %d" id))
                       {:content (format "Pinned item %d" id)})

                     (= action-type :unpin-item)
                     (let [id (:id params)]
                       (swap! ctx context/update-item id {:pinned false})
                       (println (format "[agent] UNPIN item %d" id))
                       {:content (format "Unpinned item %d" id)})

                     (= action-type :curate-memories)
                     (do (future (try (scribe/curate cfg)
                                  (catch Exception e
                                    (println (format "[agent] Curation failed: %s" (.getMessage e))))))
                         {:content "Memory curation initiated"})

                     :else
                     (try (dispatch/execute-action {:action-type action-type
                                                    :message-id (:message-id params)
                                                    :command (:command params)
                                                    :path (:path params)})
                          (catch Exception e
                            (println (format "[agent] ERROR in %s: %s" (name action-type) (.getMessage e)))
                            {:content (str "Error: " (.getMessage e))})))
            content (trunc-result (:content result))
            duplicate? (some #(= content %) (recent-result-contents ctx 3))
            ;; Always record a result: prompt.clj renders every :action as an
            ;; assistant tool_call, and a tool_call without a matching role:"tool"
            ;; message is rejected by OpenAI-compatible endpoints.
            _ (swap! ctx context/add-item :action-result
                {:caused-by action-id
                 :content (if duplicate? "(duplicate result suppressed)" content)})
            ;; Every tool that produced a result lands in the done-list.
            ;; reason and wait deliberately don't: the ledger answers "what
            ;; have I already done", and thinking or waiting is not doing.
            _ (swap! ctx context/record-action action-type params (result-ok? content) (ledger-opts cfg))]
        nil))))

(defn process-turn [ctx cfg system-prompt idle-count recently-sent]
  (try
    (let [response (call-llm ctx cfg system-prompt idle-count)]
      (if-let [actions (seq (parse-tool-calls response))]
        (loop [actions actions wait-info nil productive? false]
          (if-let [action (first actions)]
            (let [result (execute-and-record ctx cfg action recently-sent)
                  productive? (or productive?
                                (and (not= :wait (:action-type action))
                                     (not= :reason (:action-type action))))]
              (recur (rest actions) (or wait-info result) productive?))
          {:delay (:delay wait-info)
           :productive? productive?
           :waiting? (boolean (:deliberate-wait? wait-info))}))
      {:delay nil :productive? false}))
    (catch Exception e
      (println (format "[agent] Turn error: %s" (.getMessage e)))
      {:delay default-delay :productive? false})))

(defn start-message-watcher [ctx cfg monitor]
  (matrix/sync-loop ctx cfg monitor))

(defn run [cfg]
  (let [_ (System/setProperty "user.dir" (or (:home-dir cfg) "/home/wayfinder"))
        ctx (atom (or (load-context cfg) {:items [] :next-id 0 :ledger []}))
        ;; On a genuinely fresh boot, orient the resident with its own
        ;; long-term memory index so rebirth starts from what it knows,
        ;; not from zero.
        _ (when (empty? (:items @ctx))
            (try
              (let [index (scribe/list-memories cfg)]
                (when (and index (not= index "No memories stored"))
                  (swap! ctx context/add-item :memory
                    {:content (str "Long-term memory index (from before this boot):\n" index)})
                  (println "[agent] Fresh boot: injected long-term memory index")))
              (catch Exception e
                (println (format "[agent] memory orientation failed: %s" (.getMessage e))))))
        system-prompt (load-system-prompt (or (:prompts-dir cfg) "prompts"))
        monitor (Object.)
        threshold (or (:compact-threshold cfg) 8000)
        target (or (:compact-target cfg) 5000)
        cooldown-ms (* (or (:compact-cooldown cfg) 120) 1000)
        last-compact (atom (System/currentTimeMillis))
        curate-interval (* (or (:curate-interval cfg) 1800) 1000)
        last-curate (atom (System/currentTimeMillis))
        idle-count (atom 0)
        recently-sent (atom [])]
    (start-message-watcher ctx cfg monitor)
    (println (format "Wayfinder agent running. Connected to Matrix. Compact threshold=%d tokens target=%d tokens cooldown=%ds curate-interval=%ds"
               threshold target (or (:compact-cooldown cfg) 120) (or (:curate-interval cfg) 1800)))
    (loop [delay default-delay]
      (let [start (System/currentTimeMillis)]
        (try
          (locking monitor (.wait monitor delay))
          (catch InterruptedException _))
        (let [item-count (count (context/fetch-context @ctx))
              needs-compact (context/needs-compact? @ctx threshold)
              elapsed-since-compact (- start @last-compact)
              can-compact (> elapsed-since-compact cooldown-ms)
              elapsed-since-curate (- start @last-curate)
              can-curate (> elapsed-since-curate curate-interval)]
          (when (and needs-compact can-compact)
            (println (format "[agent] Context at %d items (threshold %d), triggering compaction"
                       item-count threshold))
            (reset! last-compact start)
            (try
              (compactor/compact ctx cfg target)
              (catch Exception e
                (println (format "[agent] Compaction failed: %s" (.getMessage e))))))
          (when can-curate
            (println (format "[agent] %ds since last curation, triggering memory curation"
                       (int (/ elapsed-since-curate 1000))))
            (reset! last-curate start)
            (future
              (try
                (scribe/curate cfg)
                (catch Exception e
                  (println (format "[agent] Curation failed: %s" (.getMessage e)))))))
          (let [next-result (process-turn ctx cfg system-prompt @idle-count recently-sent)]
            (cond
              (:productive? next-result) (reset! idle-count 0)
              ;; deliberate long wait: hold the counter — patience is a
              ;; chosen state, not accumulating idleness
              (:waiting? next-result) nil
              :else (swap! idle-count inc))
            (save-context ctx cfg)
            (dump-context ctx cfg)
            (recur (or (:delay next-result) default-delay))))))))
