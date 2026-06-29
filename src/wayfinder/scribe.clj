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

(defn- embedding-config [cfg]
  (let [embed-cfg (:embeddings cfg)]
    {:model (:model embed-cfg)
     :base-url (:embeddings-base-url cfg)}))

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

(defn- sidecar-path [md-path]
  (let [base (if (.endsWith md-path ".md")
               (subs md-path 0 (- (count md-path) 3))
               md-path)]
    (str base ".json")))

(defn- write-embedding-sidecar [dir md-path content cfg]
  (when-let [embed-model (:model (embedding-config cfg))]
    (try
      (let [embedding (llm/embed (:base-url cfg) (:api-key cfg) embed-model content
                      (:embeddings-base-url (embedding-config cfg)))]
        (when embedding
          (let [sidecar (File. dir (sidecar-path md-path))
                data (json/generate-string {:embedding embedding :summary (first (clojure.string/split-lines content))})]
            (.mkdirs (.getParentFile sidecar))
            (spit sidecar data))))
      (catch Exception e
        (println (format "[scribe] Embedding failed for %s: %s" md-path (.getMessage e)))))))

(defn- write-memory-file [dir filename content cfg]
  (let [f (File. dir filename)]
    (.mkdirs (.getParentFile f))
    (spit f content)
    (write-embedding-sidecar dir filename content cfg)))

(defn- delete-memory-file [dir path]
  (let [f (File. dir path)
        sidecar (File. dir (sidecar-path path))]
    (when (.exists f) (.delete f))
    (when (.exists sidecar) (.delete sidecar))))

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

(defn- extract-content [data]
  (cond
    (not (map? data)) (str data)
    (:content data) (:content data)
    (:action-type data) (let [params (dissoc (:params data) :call-id)]
                          (format "%s %s" (name (:action-type data)) (pr-str params)))
    :else (pr-str data)))

(defn- format-item [item]
  (let [content (or (extract-content (:data item)) "(no content)")
        tag (if (:remembered item) "REMEMBER" "FORGET")]
    (format "[%d] %s/%s — %s"
      (:id item)
      (name (:type item))
      tag
      content)))

(defn- execute-scribe-action [dir cfg action]
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
                      {:content (do (write-memory-file dir (:filename params) (:content params) cfg)
                                    "Memory written")})
      :delete-memory (do
                       (println (format "[scribe] DELETE %s" (:path params)))
                       {:content (do (delete-memory-file dir (:path params))
                                     "Memory deleted")})
      {:content "Unknown action"})))

(defn- run-scribe-turn [cfg dir messages]
  (let [agent-cfg (get-in cfg [:agents :scribe])
        response (llm/complete (:base-url cfg) (:api-key cfg)
                   (:model agent-cfg) messages tools/scribe-tool-definitions (:reasoning-effort agent-cfg))]
    (if-let [actions (seq (parse-scribe-calls response))]
      (do
        (println (format "[scribe] LLM returned %d actions: %s"
                   (count actions)
                   (->> actions (map (comp name :action-type)) (clojure.string/join ", "))))
        (loop [actions actions results []]
          (if-let [action (first actions)]
            (let [result (execute-scribe-action dir cfg action)]
              (recur (rest actions) (conj results result)))
            results)))
      (do
        (println (format "[scribe] LLM returned no tool calls. Content: %s" (trunc (or (:content response) "(nil)") 200)))
        []))))

(defn file-memories [cfg items]
  (println (format "[scribe] file-memories called with %d items" (count items)))
  (doseq [item items]
    (println (format "[scribe]   item %d: %s/%s — %s"
               (:id item) (name (:type item))
               (if (:remembered item) "REMEMBER" "FORGET")
               (trunc (or (extract-content (:data item)) "(nil)") 100))))
  (let [dir (ensure-dir (memory-dir cfg))
        items-str (->> items (map format-item) (clojure.string/join "\n"))
        index (scan-index dir)
        index-str (->> index
                       (map #(str (:path %) " — " (:summary %)))
                       (clojure.string/join "\n"))
        messages [{:role "system"
                   :content (str "You are the Scribe. Your ONLY job is to write memory files. You receive items and you MUST write them to disk.\n\n"
                                 "Rules:\n"
                                 "- REMEMBER items MUST be written. No exceptions.\n"
                                 "- FORGET items MUST be written unless they are truly empty or are pure error messages with no informational content.\n"
                                 "- Greetings, acknowledgments, \"message sent\" confirmations — still write these if they contain any factual content about the system or conversation.\n"
                                 "- Merge related items into one file. Write unrelated items to separate files.\n"
                                 "- First line of every file: a one-line summary.\n"
                                 "- Filenames by topic: 'system/hostname.md', 'facts/admin-name.md', 'exploration/findings.md'.\n"
                                 "- Do NOT just list memories. WRITE files. Use write-memory for every item you receive.\n\n"
                                 "Existing memory index:\n" (or index-str "No memories stored"))}
                  {:role "user"
                   :content (str "Write these items to memory:\n\n" items-str)}]]
    (let [results (run-scribe-turn cfg dir messages)]
      (println (format "[scribe] file-memories completed, %d actions executed" (count results)))
      results)))

;; --- Embedding-based recall ---

(defn- load-embeddings [dir]
  (let [base (.toPath (File. dir))
        files (->> (file-seq (File. dir))
                   (filter #(.isFile %))
                   (filter #(re-find #"\.json$" (.getName %))))]
    (for [f files]
      (try
        (let [rel (.toString (.relativize base (.toPath f)))
              data (json/parse-string (slurp f) true)]
          {:path rel
           :embedding (:embedding data)
           :summary (:summary data)})
        (catch Exception e
          (println (format "[scribe] Failed to load embedding %s: %s" (.getPath f) (.getMessage e)))
          nil)))))

(defn- dot-product [a b]
  (reduce + (map * a b)))

(defn- magnitude [v]
  (Math/sqrt (dot-product v v)))

(defn cosine-sim [a b]
  (let [ma (magnitude a)
        mb (magnitude b)]
    (if (or (zero? ma) (zero? mb))
      0.0
      (/ (dot-product a b) (* ma mb)))))

(defn recall [ctx cfg query]
  (println (format "[scribe] RECALL query: %s" (trunc query 100)))
  (let [dir (ensure-dir (memory-dir cfg))
        embed-cfg (embedding-config cfg)]
    (if-let [embed-model (:model embed-cfg)]
      (try
        (let [query-embedding (llm/embed (:base-url cfg) (:api-key cfg) embed-model query
                               (:embeddings-base-url embed-cfg))]
          (when query-embedding
            (let [stored (->> (load-embeddings dir) (remove nil?) vec)
                  _ (println (format "[scribe] Comparing against %d stored embeddings" (count stored)))
                  scored (->> stored
                              (filter :embedding)
                              (map (fn [{:keys [path embedding summary]}]
                                     {:path path
                                      :summary summary
                                      :score (cosine-sim query-embedding embedding)}))
                              (sort-by :score >)
                              (take 5))
                  _ (println (format "[scribe] Top results: %s"
                              (->> scored (map #(format "%s (%.3f)" (:path %) (:score %))) (clojure.string/join ", "))))
                  contents (doall (for [{:keys [path]} scored]
                             (read-memory-file dir (str (.replaceAll path "\\.json$" ".md")))))]
              (if (seq contents)
                (let [content (clojure.string/join "\n\n" contents)]
                  (println (format "[scribe] RECALL returned %d results, %d chars" (count contents) (count content)))
                  (swap! ctx context/add-item :memory {:content content}))
                (println "[scribe] RECALL: no embeddings found, no results")))))
        (catch Exception e
          (println (format "[scribe] RECALL embedding error: %s" (.getMessage e)))))
      (println "[scribe] RECALL: no embedding model configured"))))

;; --- Memory curation ---

(defn curate [cfg]
  (println "[scribe] CURATE: starting memory curation pass")
  (let [dir (ensure-dir (memory-dir cfg))
        index (scan-index dir)
        index-str (->> index
                       (map #(str (:path %) " — " (:summary %)))
                       (clojure.string/join "\n"))
        messages [{:role "system"
                   :content (str "You are the Scribe performing memory curation. Your job is to review all stored memories and clean them up.\n\n"
                                 "Guidelines:\n"
                                 "- List all memories first, then read any you need to examine.\n"
                                 "- MERGE: If two or more files cover the same topic or contain overlapping information, write a single consolidated file and delete the originals.\n"
                                 "- DELETE: Remove files that contain no useful information (empty, trivial, redundant, or purely conversational with no factual content).\n"
                                 "- PRUNE: If a file contains stale or outdated information alongside useful info, rewrite it with only current content and delete the old version.\n"
                                 "- QUALITY: Every file should have a clear one-line summary as its first line. Rewrite files that lack this.\n"
                                 "- Do NOT delete without reading. Do NOT merge without understanding the content.\n"
                                 "- Be thorough but conservative. When in doubt, keep.\n\n"
                                 "Current memory index:\n" (or index-str "No memories stored"))}
                  {:role "user"
                   :content "Review all memories and perform any needed curation."}]]
    (let [results (run-scribe-turn cfg dir messages)]
      (println (format "[scribe] CURATE: completed, %d actions executed" (count results)))
      results)))
