(ns wayfinder.scribe
  (:require [wayfinder.llm :as llm]
            [wayfinder.context :as context]
            [wayfinder.tools :as tools]
            [cheshire.core :as json]
            [clojure.java.io :as io])
  (:import [java.io File]))

(defn- memory-dir [cfg]
  (or (:memory-dir cfg) "/var/lib/wayfinder/memory"))

;; Compaction filing and timer-driven curation both run in futures and mutate
;; the same memory directory; serialize all scribe passes so two LLM-driven
;; passes can't interleave writes/deletes on the same files.
(defonce ^:private scribe-io-lock (Object.))

(defn- ensure-dir [dir]
  (let [f (File. dir)]
    (when-not (.exists f) (.mkdirs f))
    dir))

(defn- embedding-config [cfg]
  (let [embed-cfg (:embeddings cfg)]
    {:model (:model embed-cfg)
     :base-url (:embeddings-base-url cfg)}))

(defn- in-reserved-dir? [dir f]
  (let [abs (.getAbsolutePath f)]
    (or (.startsWith abs (.getAbsolutePath (io/file dir "archive")))
        (.startsWith abs (.getAbsolutePath (io/file dir "trash"))))))

(defn- scan-index [dir]
  (let [base (.toPath (File. dir))
        files (->> (file-seq (File. dir))
                   (filter #(.isFile %))
                   (remove #(re-find #"\.json$" (.getName %)))
                   (remove #(in-reserved-dir? dir %)))]
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
        sidecar (File. dir (sidecar-path path))
        trash-f (io/file dir "trash" path)
        trash-sidecar (io/file dir "trash" (sidecar-path path))]
    (when (.exists f)
      (.mkdirs (.getParentFile trash-f))
      (io/copy f trash-f)
      (.delete f))
    (when (.exists sidecar)
      (.mkdirs (.getParentFile trash-sidecar))
      (io/copy sidecar trash-sidecar)
      (.delete sidecar))))

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

(def ^:private max-scribe-rounds 6)

(defn- run-scribe-turn
  "Multi-round tool loop: execute the scribe's tool calls, feed the results
   back, and let it continue until it stops calling tools (or the round cap).
   A single round is not enough for curation, whose prompt instructs
   list -> read -> merge/write/delete."
  [cfg dir messages]
  (let [agent-cfg (get-in cfg [:agents :scribe])]
    (loop [messages messages round 1 all-results []]
      (let [response (llm/complete (:base-url cfg) (:api-key cfg)
                       (:model agent-cfg) messages tools/scribe-tool-definitions (:reasoning-effort agent-cfg))
            actions (seq (parse-scribe-calls response))]
        (if-not actions
          (do
            (when (= round 1)
              (println (format "[scribe] LLM returned no tool calls. Content: %s" (trunc (or (:content response) "(nil)") 200))))
            all-results)
          (let [_ (println (format "[scribe] round %d/%d: %d actions: %s"
                             round max-scribe-rounds (count actions)
                             (->> actions (map (comp name :action-type)) (clojure.string/join ", "))))
                results (mapv #(execute-scribe-action dir cfg %) actions)]
            (if (>= round max-scribe-rounds)
              (do (println "[scribe] max rounds reached, stopping")
                  (into all-results results))
              (recur (-> messages
                         (conj {:role "assistant" :content nil
                                :tool_calls (:tool_calls response)})
                         (into (map (fn [call result]
                                      {:role "tool"
                                       :tool_call_id (:id call)
                                       :content (:content result)})
                                    (:tool_calls response) results)))
                     (inc round)
                     (into all-results results)))))))))

(defn file-memories [cfg items]
  (println (format "[scribe] file-memories called with %d items" (count items)))
  (doseq [item items]
    (println (format "[scribe]   item %d: %s/%s — %s"
               (:id item) (name (:type item))
               (if (:remembered item) "REMEMBER" "FORGET")
               (trunc (or (extract-content (:data item)) "(nil)") 100))))
  (locking scribe-io-lock
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
                                 "- Filenames by topic: 'system/hostname.md', 'facts/user-name.md', 'exploration/findings.md'.\n"
                                 "- Do NOT just list memories. WRITE files. Use write-memory for every item you receive.\n\n"
                                 "Existing memory index:\n" (or index-str "No memories stored"))}
                  {:role "user"
                   :content (str "Write these items to memory:\n\n" items-str)}]]
    (let [results (run-scribe-turn cfg dir messages)]
      (println (format "[scribe] file-memories completed, %d actions executed" (count results)))
      results))))

(defn remember-note
  "Deterministic direct write from the main agent — no LLM round involved.
   Writes the memory file and its embedding sidecar, returns the filename."
  [cfg filename content]
  (locking scribe-io-lock
    (let [dir (ensure-dir (memory-dir cfg))
          filename (if (.endsWith filename ".md") filename (str filename ".md"))]
      (write-memory-file dir filename content cfg)
      (println (format "[scribe] REMEMBER wrote %s" filename))
      filename)))

(defn append-note
  "Append content to a memory file (creating it if absent) and refresh its
   embedding sidecar. Cheap running-log primitive: no whole-file rewrite."
  [cfg filename content]
  (locking scribe-io-lock
    (let [dir (ensure-dir (memory-dir cfg))
          filename (if (.endsWith filename ".md") filename (str filename ".md"))
          f (File. dir filename)
          existing (when (.exists f) (slurp f))
          merged (if existing
                   (str existing (when-not (.endsWith existing "\n") "\n") content "\n")
                   (str content "\n"))]
      (write-memory-file dir filename merged cfg)
      (println (format "[scribe] APPEND %s (+%d chars)" filename (count content)))
      filename)))

(defn move-note
  "Rename/move a memory file and its embedding sidecar."
  [cfg from to]
  (locking scribe-io-lock
    (let [dir (ensure-dir (memory-dir cfg))
          from (if (.endsWith from ".md") from (str from ".md"))
          to (if (.endsWith to ".md") to (str to ".md"))
          src (File. dir from)
          dst (File. dir to)
          src-side (File. dir (sidecar-path from))
          dst-side (File. dir (sidecar-path to))]
      (if (.exists src)
        (do
          (.mkdirs (.getParentFile dst))
          (io/copy src dst)
          (.delete src)
          (when (.exists src-side)
            (.mkdirs (.getParentFile dst-side))
            (io/copy src-side dst-side)
            (.delete src-side))
          (println (format "[scribe] MOVE %s -> %s" from to))
          {:ok? true})
        {:ok? false}))))

(defn list-memories [cfg]
  (let [dir (ensure-dir (memory-dir cfg))
        index (scan-index dir)]
    (if (seq index)
      (->> index
           (map #(str (:path %) " — " (:summary %)))
           (clojure.string/join "\n"))
      "No memories stored")))

;; --- Embedding-based recall ---

(defn- load-embeddings [dir]
  (let [base (.toPath (File. dir))
        files (->> (file-seq (File. dir))
                   (filter #(.isFile %))
                   (filter #(re-find #"\.json$" (.getName %)))
                   (remove #(in-reserved-dir? dir %)))]
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

(defn- score-memories
  "Score every stored sidecar against an embedding, best match first."
  [dir embedding]
  (let [stored (->> (load-embeddings dir) (remove nil?) vec)]
    (println (format "[scribe] Comparing against %d stored embeddings" (count stored)))
    (->> stored
         (filter :embedding)
         (map (fn [stored-memory]
                {:path (:path stored-memory)
                 :summary (:summary stored-memory)
                 :score (cosine-sim embedding (:embedding stored-memory))}))
         (sort-by :score >))))

(defn recall [ctx cfg query]
  (println (format "[scribe] RECALL query: %s" (trunc query 100)))
  (let [dir (ensure-dir (memory-dir cfg))
        embed-cfg (embedding-config cfg)]
    (if-let [embed-model (:model embed-cfg)]
      (try
        (let [query-embedding (llm/embed (:base-url cfg) (:api-key cfg) embed-model query
                               (:embeddings-base-url embed-cfg))]
          (when query-embedding
            (let [scored (take 5 (score-memories dir query-embedding))
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

;; --- Passive recall cues ---
;; recall is deliberate: it only fires when the agent already suspects it knows
;; something. This is the involuntary counterpart — an incoming percept is
;; embedded once and compared against the memory index, and a close match
;; becomes an ordinary context item. No LLM call, no pinning, no priority: the
;; cue is prunable and compactable like anything else, and can be ignored.

(def ^:private default-cue-threshold 0.75)
(def ^:private default-cue-max 2)
(def ^:private default-cue-cooldown 50)

(defn- cue-config [cfg]
  {:enabled? (not (false? (:cues-enabled? cfg)))
   :threshold (or (:cue-threshold cfg) default-cue-threshold)
   :max-cues (or (:cue-max cfg) default-cue-max)
   :cooldown (or (:cue-cooldown-items cfg) default-cue-cooldown)})

(def ^:private cue-stamp
  (java.time.format.DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss"))

(defn- now-stamp []
  (.format (java.time.LocalDateTime/now) cue-stamp))

(defn- cue-text [summary path]
  (format "⟪memory cue: you have a memory that may relate — \"%s\" (%s). Recall it if useful; ignore if not.⟫"
    (trunc (or summary "(no summary)") 200) path))

(defn- cue-due?
  "Cooldown, counted in context items rather than wall-clock: the same file is
   not cued again until `cooldown` further items have accumulated, so one
   recurring topic cannot keep re-announcing itself."
  [ctx-val path cooldown]
  (let [cued-at (get (:cue-log ctx-val) path)]
    (or (nil? cued-at) (>= (- (:next-id ctx-val) cued-at) cooldown))))

(defn cue-memories
  "Embed a percept and, when a stored memory scores close enough, drop a short
   cue into context. Best-effort by design: no embedding model, no match, or
   any failure at all means one log line and nothing else — this path never
   surfaces an error to the agent."
  [ctx cfg text]
  (let [{:keys [enabled? threshold max-cues cooldown]} (cue-config cfg)
        embed-cfg (embedding-config cfg)
        embed-model (:model embed-cfg)]
    (when (and enabled? embed-model (seq (str text)))
      (try
        (let [dir (ensure-dir (memory-dir cfg))
              embedding (llm/embed (:base-url cfg) (:api-key cfg) embed-model (str text)
                          (:embeddings-base-url embed-cfg))]
          (when embedding
            (let [scored (score-memories dir embedding)
                  candidates (->> scored
                                  (filter #(>= (:score %) threshold))
                                  (map #(assoc % :md-path (.replaceAll (str (:path %)) "\\.json$" ".md")))
                                  (filter #(cue-due? @ctx (:md-path %) cooldown))
                                  (take max-cues))]
              ;; A cue that fires invisibly cannot be audited: score, memory and
              ;; timestamp go both to the log and into the item's data, so a
              ;; context dump answers "was that a cue?" on its own. Not a ledger
              ;; entry — like reason and wait, being reminded of something is
              ;; not doing something.
              (doseq [{:keys [md-path summary score]} candidates]
                (let [at (now-stamp)]
                  (println (format "[scribe] CUE FIRED %s memory=%s score=%.3f threshold=%.2f"
                             at md-path score (double threshold)))
                  (swap! ctx (fn [c]
                               (-> c
                                   (context/add-item :memory-cue {:content (cue-text summary md-path)
                                                                  :memory md-path
                                                                  :score score
                                                                  :at at})
                                   (assoc-in [:cue-log md-path] (:next-id c)))))))
              (when (and (empty? candidates) (seq scored))
                (println (format "[scribe] CUE none %s best=%s score=%.3f threshold=%.2f"
                           (now-stamp) (:path (first scored)) (:score (first scored)) (double threshold)))))))
        (catch Exception e
          (println (format "[scribe] CUE skipped: %s" (.getMessage e))))))))

;; --- Memory curation ---

(defn curate [cfg]
  (println "[scribe] CURATE: starting memory curation pass")
  (locking scribe-io-lock
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
      results))))
