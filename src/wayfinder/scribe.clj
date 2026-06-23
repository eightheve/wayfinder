(ns wayfinder.scribe
  (:require [wayfinder.llm :as llm]
            [wayfinder.context :as context]
            [wayfinder.tools :as tools]
            [cheshire.core :as json]
            [clojure.java.io :as io])
  (:import [java.io File]))

(defn- memory-dir [cfg]
  (or (:memory-dir cfg) "/var/lib/wayfinder/memory"))

(defn- ensure-dir [dir]
  (let [f (File. dir)]
    (when-not (.exists f) (.mkdirs f))
    dir))

(defn- scan-index [dir]
  (let [base (.toPath (File. dir))
        files (->> (file-seq (File. dir))
                   (filter #(.isFile %))
                   (remove #(re-find #"\.json$" (.getName %))))]
    (for [f files]
      (let [rel (.toString (.relativize base (.toPath f)))
            first-line (with-open [rdr (clojure.java.io/reader f)]
                         (first (line-seq rdr)))]
        {:path rel :summary (or first-line "(empty)")}))))

(defn- read-memory-file [dir path]
  (let [f (File. dir path)]
    (if (.exists f) (slurp f) "File not found")))

(defn- write-memory-file [dir filename content]
  (let [f (File. dir filename)]
    (.mkdirs (.getParentFile f))
    (spit f content)))

(defn- delete-memory-file [dir path]
  (let [f (File. dir path)]
    (when (.exists f) (.delete f))))

(defn- parse-scribe-calls [response]
  (when-let [calls (:tool_calls response)]
    (for [call calls]
      (let [func (:function call)]
        {:action-type (keyword (:name func))
         :params (try (json/parse-string (:arguments func) true)
                      (catch Exception _ {}))}))))

(defn- execute-scribe-action [dir action]
  (let [{:keys [action-type params]} action]
    (case action-type
      :list-memories {:content (if-let [index (seq (scan-index dir))]
                                 (->> index
                                      (map #(str (:path %) " — " (:summary %)))
                                      (clojure.string/join "\n"))
                                 "No memories stored")}
      :read-memory {:content (read-memory-file dir (:path params))}
      :write-memory {:content (do (write-memory-file dir (:filename params) (:content params))
                                  "Memory written")}
      :delete-memory {:content (do (delete-memory-file dir (:path params))
                                   "Memory deleted")}
      {:content "Unknown action"})))

(defn- run-scribe-turn [cfg dir messages]
  (let [response (llm/complete (:base-url cfg) (:api-key cfg)
                   (get-in cfg [:models :small]) messages tools/scribe-tool-definitions)]
    (if-let [actions (seq (parse-scribe-calls response))]
      (loop [actions actions results []]
        (if-let [action (first actions)]
          (let [result (execute-scribe-action dir action)]
            (recur (rest actions) (conj results result)))
          results))
      [])))

(defn file-memories [cfg items]
  (let [dir (ensure-dir (memory-dir cfg))
        items-str (->> items
                       (map (fn [item]
                              (format "[%d] %s — %s"
                                (:id item)
                                (name (:type item))
                                (pr-str (:data item)))))
                       (clojure.string/join "\n"))
        index (scan-index dir)
        index-str (->> index
                       (map #(str (:path %) " — " (:summary %)))
                       (clojure.string/join "\n"))
        messages [{:role "system"
                   :content (str "You are the Scribe, a memory filing agent. You receive items that have been forgotten from the main agent's context. "
                                 "Decide whether each item is worth keeping in long-term memory. If it is, file it as a memory. "
                                 "If it duplicates or extends an existing memory, read it first and then overwrite it with merged content. "
                                 "If the item is not worth keeping, simply do nothing.\n\n"
                                 "Current memory index:\n" (or index-str "No memories stored"))}
                  {:role "user"
                   :content (str "Forgotten items to process:\n\n" items-str)}]]
    (run-scribe-turn cfg dir messages)))

(defn recall [ctx cfg query]
  (let [dir (ensure-dir (memory-dir cfg))
        index (scan-index dir)
        index-str (->> index
                       (map #(str (:path %) " — " (:summary %)))
                       (clojure.string/join "\n"))
        messages [{:role "system"
                   :content (str "You are the Scribe, a memory retrieval agent. The main agent is asking to recall information. "
                                 "Search the memory index for relevant files, read them, and return the relevant content. "
                                 "Current memory index:\n" (or index-str "No memories stored"))}
                  {:role "user"
                   :content (str "Recall query: " query)}]
        results (run-scribe-turn cfg dir messages)]
    (when (seq results)
      (swap! ctx context/add-item :memory
        {:content (->> results
                       (map :content)
                       (remove #(= "No memories stored" %))
                       (clojure.string/join "\n\n"))}))))
