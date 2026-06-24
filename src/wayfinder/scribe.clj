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

(defn- trunc [s max-len]
  (let [s (str s)]
    (if (> (count s) max-len)
      (str (subs s 0 max-len) "...")
      s)))

(defn- execute-scribe-action [dir action]
  (let [{:keys [action-type params]} action]
    (case action-type
      :list-memories {:content (if-let [index (seq (scan-index dir))]
                                 (->> index
                                      (map #(str (:path %) " — " (:summary %)))
                                      (clojure.string/join "\n"))
                                 "No memories stored")}
      :read-memory (do
                     (println (format "[scribe] READ %s" (:path params)))
                     {:content (read-memory-file dir (:path params))})
      :write-memory (do
                      (println (format "[scribe] WRITE %s — %s"
                                 (:filename params)
                                 (trunc (get params :content "") 120)))
                      {:content (do (write-memory-file dir (:filename params) (:content params))
                                    "Memory written")})
      :delete-memory (do
                       (println (format "[scribe] DELETE %s" (:path params)))
                       {:content (do (delete-memory-file dir (:path params))
                                     "Memory deleted")})
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
  (println (format "[scribe] file-memories called with %d items" (count items)))
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
    (let [results (run-scribe-turn cfg dir messages)]
      (println (format "[scribe] file-memories completed, %d actions executed" (count results)))
      results)))

(defn recall [ctx cfg query]
  (println (format "[scribe] RECALL query: %s" (trunc query 100)))
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
      (let [content (->> results
                         (map :content)
                         (remove #(= "No memories stored" %))
                         (clojure.string/join "\n\n"))]
        (println (format "[scribe] RECALL returned %d results, %d chars" (count results) (count content)))
        (swap! ctx context/add-item :memory {:content content})))))
