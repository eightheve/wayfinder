(ns wayfinder.scribe
  (:require [wayfinder.llm :as llm]
            [wayfinder.context :as context]
            [wayfinder.tools :as tools]
            [wayfinder.jev :as jev]
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
     :base-url (:base-url embed-cfg)
     :api-key (:api-key embed-cfg)}))

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
  (let [embed-cfg (embedding-config cfg)]
    (when-let [embed-model (:model embed-cfg)]
      (try
        (let [embedding (llm/embed (:base-url embed-cfg) (:api-key embed-cfg) embed-model content)]
          (when embedding
            (let [sidecar (File. dir (sidecar-path md-path))
                  data (json/generate-string {:embedding embedding :summary (first (clojure.string/split-lines content))})]
              (.mkdirs (.getParentFile sidecar))
              (spit sidecar data))))
        (catch Exception e
          (println (format "[scribe] Embedding failed for %s: %s" md-path (.getMessage e))))))))

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

;; Cap shared by Jev summary ranking (below) and Jev filing routing: both fan
;; path+summary judgments out to Jev, bounded and chunked per call.
(def ^:private jev-scoring-max-memories 100)

;; Filing states carry full item lines (not summaries), so both bounds here are
;; tighter than elsewhere: states must stay well under Jev's ~32k-token request
;; budget, and full untruncated content made a 60-item chunk blow it (real log
;; failures). Every line is truncated and chunks are capped at 20 items.
(def ^:private jev-filing-chunk-size 20)

;; --- Jev filing routing ---
;; Compaction used to hand every item to the Scribe LLM even when it plainly
;; belongs in an existing file. Jev's closed-set choice is a cheap first hop:
;; route each item at an existing file (or "new-file"), and only the items it
;; declined reach the LLM. A nil return means no opinion — Jev unavailable,
;; failed, or no chunk answered — and the whole batch falls through to the
;; LLM pass: exactly today's behavior.

(defn- jev-file-routes
  "Route compacted items at existing memory files: one Jev choice per item —
   which indexed file should absorb it, or \"new-file\". Returns
   {item-id {:path <file path or \"new-file\"> :confidence n}} or nil when Jev
   has no opinion at all."
  [cfg items index]
  (when (and (jev/available? cfg) (seq items) (seq index))
    (try
      (let [idx (vec (take jev-scoring-max-memories index))
            threshold (or (:jev-filing-threshold cfg) 0.6)
            criteria (into {"new-file" "None of the existing files fits — this needs its own new memory file."}
                           (map (fn [{:keys [path summary]}] [path (trunc summary 100)]))
                           idx)
            answered (atom false)
            routes
            (reduce into {}
              (keep
                (fn [chunk]
                  (let [state (str "MEMORY FILES (path — summary):\n"
                                   (clojure.string/join "\n"
                                     (map #(str (:path %) " — " (:summary %)) idx))
                                   "\n\nITEMS TO FILE:\n"
                                   (clojure.string/join "\n" (map #(trunc (format-item %) 350) chunk)))
                        questions (into {}
                                    (map (fn [item]
                                           ;; The question id is never shown to the model —
                                           ;; the numeric item id must be in the instructions.
                                           [(str "i_" (:id item))
                                            {:type "choice"
                                             :instructions (format "Item %d above must be filed to long-term memory. Which existing file should absorb it, or does it need a new file? Route to the single best-fitting file by topic." (:id item))
                                             :criteria criteria}])
                                         chunk))]
                    (when-let [result (jev/evaluate cfg state questions)]
                      (reset! answered true)
                      (into {}
                        (keep (fn [item]
                                (let [answer (get (:answers result) (str "i_" (:id item)))
                                      path (jev/choice-of (:answers result) (str "i_" (:id item)))]
                                  ;; Below-threshold (or missing) confidence means
                                  ;; 'no opinion': the item goes back to the LLM.
                                  (when (and path (:confidence answer) (>= (:confidence answer) threshold))
                                    [(:id item) {:path path :confidence (double (:confidence answer))}])))
                              chunk)))))
                (partition-all jev-filing-chunk-size (vec items))))]
        (when @answered routes))
      (catch Exception e
        (println (format "[scribe] Jev filing failed: %s" (.getMessage e)))
        nil))))

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

(def ^:private max-scribe-rounds 12)

(defn- run-scribe-turn
  "Multi-round tool loop: execute the scribe's tool calls, feed the results
   back, and let it continue until it stops calling tools (or the round cap).
   A single round is not enough for curation, whose prompt instructs
   list -> read -> merge/write/delete."
  [cfg dir messages]
  (let [agent-cfg (get-in cfg [:agents :scribe])]
    (loop [messages messages round 1 all-results []]
      (let [response (llm/complete (:base-url agent-cfg) (:api-key agent-cfg)
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

;; append-note is defined below with the other direct-write primitives; its
;; reentrant locking makes it safe to call from inside this function's lock.
(declare append-note)

(defn file-memories [cfg items]
  (println (format "[scribe] file-memories called with %d items" (count items)))
  (doseq [item items]
    (println (format "[scribe]   item %d: %s/%s — %s"
               (:id item) (name (:type item))
               (if (:remembered item) "REMEMBER" "FORGET")
               (trunc (or (extract-content (:data item)) "(nil)") 100))))
  (locking scribe-io-lock
    (let [dir (ensure-dir (memory-dir cfg))
          index (scan-index dir)
          index-str (->> index
                         (map #(str (:path %) " — " (:summary %)))
                         (clojure.string/join "\n"))
          ;; Jev routes first; only the items it declined (new-file route,
          ;; below-threshold confidence, or no route at all) reach the Scribe
          ;; LLM. routes nil is the fail-open path: the LLM pass sees every
          ;; item, exactly as before.
          routes (jev-file-routes cfg items index)
          routed (if routes
                   (filter (fn [item]
                             (let [route (get routes (:id item))]
                               (and route (not= "new-file" (:path route)))))
                           items)
                   [])
          leftovers (if routes
                      (filter (fn [item]
                                (let [route (get routes (:id item))]
                                 (or (nil? route) (= "new-file" (:path route)))))
                              items)
                      items)
          ;; append-note takes scribe-io-lock itself; clojure locking is
          ;; reentrant, so calling it under this lock is safe — no bypass.
          routed-results
          (mapv (fn [item]
                  (let [{:keys [path confidence]} (get routes (:id item))]
                    (println (format "[scribe] ROUTE item %d → %s (Jev, confidence %.2f)"
                               (:id item) path (double confidence)))
                    (append-note cfg path (extract-content (:data item)))))
                routed)]
      (if (and routes (empty? leftovers))
        (do
          (println (format "[scribe] Jev routed all %d items — LLM pass skipped" (count routed)))
          routed-results)
        (let [items-str (->> leftovers (map format-item) (clojure.string/join "\n"))
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
                         :content (str "Write these items to memory:\n\n" items-str)}]
              results (concat routed-results (run-scribe-turn cfg dir messages))]
          (println (format "[scribe] file-memories completed, %d actions executed" (count results)))
          results)))))

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

;; --- Hybrid ranking (embeddings + Jev) ---
;; The two sources grade the same path+summary lines independently and err
;; differently: embeddings catch lexical echoes, Jev catches paraphrase and
;; intent. blend-scores rewards memories both sources back (0.6*max + 0.4*min),
;; lets either source decide alone when the other is silent, and drops an
;; entry when neither has an opinion.

(defn- embed-scores
  "Sidecar similarities for a query as {md-path score}. nil when the embedding
   call fails — the map form lines up with Jev's {path p} for blending."
  [dir embed-cfg text]
  (try
    (when-let [embedding (llm/embed (:base-url embed-cfg) (:api-key embed-cfg)
                                   (:model embed-cfg) text)]
      (into {}
        (for [{:keys [path score]} (score-memories dir embedding)]
          [(.replaceAll (str path) "\\.json$" ".md") (double score)])))
    (catch Exception e
      (println (format "[scribe] Embedding scoring failed: %s" (.getMessage e)))
      nil)))

(defn- jev-summary-scores
  "One Jev call per chunk of memories: one noul per memory — 'is this memory
   relevant to the query?' — judged from path+summary only. Returns {md-path p}
   or nil on total failure; a failed chunk just contributes nothing."
  [cfg query index]
  (when (and (jev/available? cfg) (seq index))
    (try
      (let [mems (vec (take jev-scoring-max-memories index))]
        (reduce into {}
          (keep
            (fn [chunk]
              (let [index-str (clojure.string/join "\n"
                               (map #(str (:path %) " — " (:summary %)) chunk))
                   state (str "QUERY:\n" query
                              "\n\nMEMORY INDEX (path — summary):\n" index-str)
                   questions (into {}
                               (map-indexed
                                 (fn [i {:keys [path summary]}]
                                   [(str "m_" i)
                                    {:type "noul"
                                     :instructions (str "Does the memory '" path "' — '" summary
                                                        "' — possibly contain information relevant to the query? "
                                                        "Judge only from the path and summary.")
                                     :criteria {"true" "Path or summary suggests relevant content"
                                                "false" "Unrelated to the query"}}]))
                               chunk)]
                (when-let [result (jev/evaluate cfg state questions)]
                 (into {}
                   (keep-indexed
                     (fn [i {:keys [path]}]
                       (when-let [p (jev/noul-of (:answers result) (str "m_" i))]
                         [path (double p)]))
                     chunk)))))
            (jev/chunk-batch mems))))
      (catch Exception e
        (println (format "[scribe] Jev scoring failed: %s" (.getMessage e)))
        nil))))

(defn- blend-scores
  "Fuse Jev probability and embedding similarity per memory. Both sources:
   0.6*max + 0.4*min (agreement strengthens, disagreement penalizes); a single
   source decides alone."
  [jev-scores embed-scores]
  (into {}
    (comp
      (map (fn [path]
             (let [p (get jev-scores path)
                  sim (get embed-scores path)
                  score (cond
                          (and p sim) (+ (* 0.6 (max p sim)) (* 0.4 (min p sim)))
                          p (double p)
                          sim (double sim)
                          :else 0.0)]
               (when (pos? score) [path score]))))
      (filter some?))
    (distinct (concat (keys jev-scores) (keys embed-scores)))))

;; --- Deliberate recall ---
;; Two stages. Stage 1 ranks path+summary lines cheaply (embeddings ∥ Jev) and
;; takes the union of the blended top 6 with anything either source backs at
;; ≥ 0.7 on its own. Stage 2 sends full texts to Jev for a per-memory
;; 'present or omit' verdict — summaries that oversold their contents get
;; dropped here. Every failure degrades one level: no Jev → stage 1 only;
;; no embeddings → Jev only; neither → log and no-op.
(defn recall [ctx cfg query]
  (println (format "[scribe] RECALL query: %s" (trunc query 100)))
  (let [dir (ensure-dir (memory-dir cfg))
        embed-cfg (embedding-config cfg)
        embed-model (:model embed-cfg)
        jev? (jev/available? cfg)]
    (if (or embed-model jev?)
      (try
        (let [index (scan-index dir)
              embed-scores (when embed-model (embed-scores dir embed-cfg query))
              jev-scores (jev-summary-scores cfg query index)
              blended (blend-scores jev-scores embed-scores)
              ranked (sort-by val > blended)
              strong (filter (fn [path]
                               (or (>= (get jev-scores path 0.0) 0.7)
                                   (>= (get embed-scores path 0.0) 0.7)))
                        (keys blended))
              candidates (vec (take 8 (distinct (concat (map key (take 6 ranked)) strong))))
              _ (println (format "[scribe] RECALL stage 1: %d candidates (jev=%s, embed=%s)"
                           (count candidates) (boolean jev-scores) (boolean embed-scores)))
              presented
              (if (and jev? (seq candidates))
                (let [texts (into {}
                              (map (fn [p] [p (trunc (read-memory-file dir p) 4000)]))
                              candidates)
                      state (str "QUERY:\n" query
                                 "\n\nCANDIDATE MEMORIES (full text):\n\n"
                                 (clojure.string/join "\n\n"
                                   (map (fn [[p t]] (str "=== " p " ===\n" t)) texts)))
                      questions (into {}
                                  (map-indexed
                                    (fn [i p]
                                      [(str "m_" i)
                                       {:type "noul"
                                        :instructions (str "Should the memory '" p "' be included in the agent's recall results for its query? Does the full text under '=== " p " ===' contain information relevant to the query?")
                                        :criteria {"true" "Directly useful for answering the query"
                                                   "false" "Only superficially related or irrelevant"}}])
                                  candidates))]
                  (if-let [result (jev/evaluate cfg state questions)]
                    ;; A no-opinion call hands the decision back to stage 1.
                    (let [verdicts (keep-indexed
                                     (fn [i p]
                                       (when-let [v (jev/noul-of (:answers result) (str "m_" i))]
                                         [p (double v)]))
                                     candidates)
                          picked (->> verdicts
                                      (filter #(>= (second %) 0.5))
                                      (sort-by second >)
                                      (take 5)
                                      (map first)
                                      vec)]
                      (println (format "[scribe] RECALL stage 2: Jev kept %d of %d candidates"
                                 (count picked) (count candidates)))
                      picked)
                    (vec (take 5 candidates))))
                (vec (take 5 candidates)))]
          (println (format "[scribe] Top results: %s"
                     (if (seq presented)
                       (->> presented
                            (map #(format "%s (%.3f)" % (double (get blended % 0.0))))
                            (clojure.string/join ", "))
                       "(none)")))
          (if (seq presented)
            (let [contents (doall (map #(read-memory-file dir %) presented))
                  content (clojure.string/join "\n\n" contents)]
              (println (format "[scribe] RECALL returned %d results, %d chars"
                         (count contents) (count content)))
              (swap! ctx context/add-item :memory {:content content}))
            (println "[scribe] RECALL: no candidates above the bar")))
        (catch Exception e
          (println (format "[scribe] RECALL error: %s" (.getMessage e)))))
      (println "[scribe] RECALL: neither Jev nor an embedding model configured"))))

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
  "Rank stored memories against an incoming percept and, when one scores close
   enough, drop a short cue into context. Ranking is the stage-1 hybrid:
   embedding similarity and Jev's probabilistic judgment grade the same
   path+summary lines independently, and the blend resolves disagreement in
   favor of agreement. Best-effort by design: no source available, no match,
   or any failure at all means one log line and nothing else — this path
   never surfaces an error to the agent."
  [ctx cfg text]
  (let [{:keys [enabled? threshold max-cues cooldown]} (cue-config cfg)
        embed-cfg (embedding-config cfg)
        embed-model (:model embed-cfg)
        jev? (jev/available? cfg)]
    (when (and enabled? (or embed-model jev?) (seq (str text)))
      (try
        (let [dir (ensure-dir (memory-dir cfg))
              index (scan-index dir)
              summaries (into {} (map (juxt :path :summary)) index)
              embed-scores (when embed-model (embed-scores dir embed-cfg (str text)))
              jev-scores (jev-summary-scores cfg (str text) index)
              blended (blend-scores jev-scores embed-scores)
              ranked (sort-by val > blended)
              candidates (->> ranked
                              (filter (fn [[_ score]] (>= score threshold)))
                              (filter #(cue-due? @ctx (key %) cooldown))
                              (take max-cues)
                              (map (fn [[path score]]
                                     {:md-path path
                                      :score score
                                      :summary (get summaries path)})))]
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
          (when (and (empty? candidates) (seq ranked))
            (println (format "[scribe] CUE none %s best=%s score=%.3f threshold=%.2f"
                       (now-stamp) (key (first ranked)) (val (first ranked)) (double threshold)))))
        (catch Exception e
          (println (format "[scribe] CUE skipped: %s" (.getMessage e))))))))

;; --- Memory curation ---

;; Curation fires on a timer; without a change detector it would re-run the
;; multi-round LLM pass over an unchanged index forever. The fingerprint is
;; stored only AFTER a pass completes, so a pass that throws leaves it unset
;; and the next tick retries.
(defonce ^:private curate-fingerprint (atom nil))

(defn- jev-curation-verdicts
  "One Jev choice per memory file — keep, delete, or rewrite — judged from
   each file's summary plus a short content preview. Returns
   {path {:verdict :keep|:delete|:rewrite :confidence n}} or nil when Jev is
   unavailable or no chunk answered; nil hands the whole pass back to the LLM
   scribe exactly as before."
  [cfg dir index]
  (when (and (jev/available? cfg) (seq index))
    (try
      (let [threshold (or (:jev-curate-threshold cfg) 0.65)
            answered (atom false)
            verdicts
            (reduce into {}
              (keep
                (fn [chunk]
                  (let [state (clojure.string/join "\n\n"
                                (map (fn [{:keys [path summary]}]
                                      (format "=== %s ===\n%s\n%s"
                                        path summary (trunc (read-memory-file dir path) 200)))
                                    chunk))
                        questions (into {}
                                   (map-indexed
                                     (fn [i {:keys [path]}]
                                       ;; The question id is never shown to the model —
                                       ;; the file path must be in the instructions itself.
                                       [(str "f_" i)
                                        {:type "choice"
                                         :instructions (format "Should the memory file '%s' be deleted (no useful/outdated content), kept as-is, or rewritten (contains useful info but is stale/messy)?" path)
                                         :criteria {"keep" "Useful and current"
                                                    "delete" "Empty, trivial, purely conversational, or fully redundant"
                                                    "rewrite" "Worth keeping but needs cleanup/merging with other content"}}])
                                     chunk))]
                    (when-let [result (jev/evaluate cfg state questions)]
                      (reset! answered true)
                      (into {}
                        (keep-indexed
                          (fn [i {:keys [path]}]
                            (let [answer (get (:answers result) (str "f_" i))
                                 choice (jev/choice-of (:answers result) (str "f_" i))]
                              ;; Below-threshold (or missing) confidence means
                              ;; 'no opinion': the file is simply left alone.
                              (when (and choice (:confidence answer) (>= (:confidence answer) threshold))
                                [path {:verdict (keyword choice) :confidence (double (:confidence answer))}])))
                          chunk)))))
                (jev/chunk-batch (vec index))))]
        (when @answered verdicts))
      (catch Exception e
        (println (format "[scribe] Jev curation failed: %s" (.getMessage e)))
        nil))))

(defn- curation-messages
  "The curation prompt in today's shape (index + guidelines + review request).
   With focus-paths set, Jev has already settled keep/delete for everything
   else, so the pass is narrowed to a focused review of just those files."
  [index focus-paths]
  (let [index-str (->> index
                       (map #(str (:path %) " — " (:summary %)))
                       (clojure.string/join "\n"))
        focus (when focus-paths
                (format "\n\nFOCUSED REVIEW: Only these files need review this pass: %s. Keep and delete decisions for every other file have already been made — do not touch them."
                        (clojure.string/join ", " focus-paths)))]
    [{:role "system"
      :content (str "You are the Scribe performing memory curation. Your job is to review all stored memories and clean them up.\n\n"
                    "Guidelines:\n"
                    "- List all memories first, then read any you need to examine.\n"
                    "- MERGE: If two or more files cover the same topic or contain overlapping information, write a single consolidated file and delete the originals.\n"
                    "- DELETE: Remove files that contain no useful information (empty, trivial, redundant, or purely conversational with no factual content).\n"
                    "- PRUNE: If a file contains stale or outdated information alongside useful info, rewrite it with only current content and delete the old version.\n"
                    "- QUALITY: Every file should have a clear one-line summary as its first line. Rewrite files that lack this.\n"
                    "- Do NOT delete without reading. Do NOT merge without understanding the content.\n"
                    "- Be thorough but conservative. When in doubt, keep.\n\n"
                    "Current memory index:\n" (or index-str "No memories stored")
                    (or focus ""))}
     {:role "user"
      :content (if focus-paths
                 (str "Review only the files listed for focused review and perform any needed curation on them."
                      " Files to review: " (clojure.string/join ", " focus-paths))
                 "Review all memories and perform any needed curation.")}]))

(defn curate [cfg]
  (println "[scribe] CURATE: starting memory curation pass")
  (locking scribe-io-lock
    (let [dir (ensure-dir (memory-dir cfg))
          index (scan-index dir)
          fingerprint (hash (mapv (juxt :path :summary) index))]
      (if (= fingerprint @curate-fingerprint)
        (do
          (println "[scribe] CURATE: index unchanged since last pass — skipping")
          [])
        (let [verdicts (jev-curation-verdicts cfg dir index)]
          (if (nil? verdicts)
            ;; No Jev opinion: the full LLM pass, byte-identical prompt as
            ;; before. The fingerprint is stored only after the pass returns;
            ;; a thrown pass leaves it unset and the next tick retries.
            (let [results (run-scribe-turn cfg dir (curation-messages index nil))]
              (reset! curate-fingerprint fingerprint)
              (println (format "[scribe] CURATE: completed, %d actions executed" (count results)))
              results)
            (let [deletes (into {} (filter (fn [[_ v]] (= :delete (:verdict v))) verdicts))
                  rewrites (vec (map key (filter (fn [[_ v]] (= :rewrite (:verdict v))) verdicts)))]
              ;; Deletes are safe to execute deterministically:
              ;; delete-memory-file moves files to trash/, nothing is lost.
              (doseq [[path {:keys [confidence]}] deletes]
                (println (format "[scribe] CURATE DELETE %s (Jev, confidence %.2f)" path (double confidence)))
                (delete-memory-file dir path))
              (if (seq rewrites)
                (let [results (run-scribe-turn cfg dir (curation-messages index rewrites))]
                  (reset! curate-fingerprint fingerprint)
                  (println (format "[scribe] CURATE: completed, %d actions executed" (count results)))
                  results)
                (do
                  (reset! curate-fingerprint fingerprint)
                  (println (format "[scribe] CURATE: Jev settled everything (%d deletions) — LLM pass skipped" (count deletes)))
                  [])))))))))
