(ns wayfinder.scribe
  (:require [wayfinder.llm :as llm]
            [wayfinder.context :as context]
            [wayfinder.tools :as tools]
            [wayfinder.jev :as jev]
            [cheshire.core :as json]
            [clojure.edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh])
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

(defn- drop-stale-sidecar
  "Delete the embedding sidecar belonging to an .md file. A missing embedding
   degrades recall gracefully; a WRONG embedding lies: a sidecar left behind
   after a failed embedding call still embeds the file's previous content,
   so recall silently scores against outdated text instead of admitting the
   gap. Best-effort: failures are logged, never thrown."
  [dir md-path]
  (try
    (let [sidecar (File. dir (sidecar-path md-path))]
      (when (.exists sidecar)
        (.delete sidecar)
        (println (format "[scribe] dropped stale sidecar for %s" md-path))))
    (catch Exception e
      (println (format "[scribe] stale sidecar cleanup failed for %s: %s"
                 md-path (.getMessage e))))))

(defn- write-embedding-sidecar [dir md-path content cfg]
  (let [embed-cfg (embedding-config cfg)]
    (when-let [embed-model (:model embed-cfg)]
      (try
        (let [embedding (llm/embed (:base-url embed-cfg) (:api-key embed-cfg) embed-model content)]
          (if embedding
            (let [sidecar (File. dir (sidecar-path md-path))
                  data (json/generate-string {:embedding embedding :summary (first (clojure.string/split-lines content))})]
              (.mkdirs (.getParentFile sidecar))
              (spit sidecar data))
            ;; nil embedding is still a failure: drop the previous content's
            ;; sidecar instead of letting recall score against stale text.
            (drop-stale-sidecar dir md-path)))
        (catch Exception e
          (println (format "[scribe] Embedding failed for %s: %s" md-path (.getMessage e)))
          (drop-stale-sidecar dir md-path))))))

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

;; Filing routes every indexed file at Jev as a Choice option, so this cap is
;; Jev's ceiling, not a preference: Choice supports at most 255 options
;; including "new-file", and the real corpus (114 files and growing) already
;; blew past the old shared cap of 100 — files beyond it were unrouteable and
;; silently always fell through to the LLM pass.
(def ^:private jev-filing-max-memories 250)

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
      (let [idx (vec (take jev-filing-max-memories index))
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
  ;; :action rides along on every result so the caller can audit which paths
  ;; a pass actually touched — coverage accounting for chunked curation.
  (assoc (let [{:keys [action-type params]} action]
           (case action-type
             :list-memories {:content (if-let [index (seq (scan-index dir))]
                                        (->> index
                                             (map #(str (:path %) " — " (:summary %)))
                                             (clojure.string/join "\n"))
                                        "No memories stored")}
             :read-memory (do
                            (println (format "[scribe] READ %s" (:path params)))
                            ;; Cap what one read can inject into the loop: the
                            ;; largest file on the real corpus measured 31k
                            ;; chars — enough to crowd out the scribe's own
                            ;; plan. The tool description already promises this
                            ;; truncation; the suffix names the total so the
                            ;; model knows what it did not see.
                            (let [content (read-memory-file dir (:path params))
                                  total (count content)]
                              {:content (if (> total 8000)
                                          (str (trunc content 8000)
                                               (format "\n...[truncated, %d chars total]" total))
                                          content)}))
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
             {:content "Unknown action"}))
         :action action))

(def ^:private max-scribe-rounds 12)

(defn- run-scribe-turn
  "Multi-round tool loop: execute the scribe's tool calls, feed the results
   back, and let it continue until it stops calling tools (or the round cap).
   A single round is not enough for curation, whose prompt instructs
   list -> read -> merge/write/delete.
   With opts {:must-cover paths :max-continue n}, an early quit becomes a
   nudge: when the model stops with no tool calls while must-cover files were
   never read, written, or deleted, it is told to finish — up to max-continue
   (default 2) times — instead of ending the pass with the assignment half
   done. Coverage counts every read-memory :path, write-memory :filename and
   delete-memory :path across all rounds."
  ([cfg dir messages] (run-scribe-turn cfg dir messages nil))
  ([cfg dir messages {:keys [must-cover max-continue] :or {max-continue 2}}]
   (let [agent-cfg (get-in cfg [:agents :scribe])
         must-cover (set must-cover)]
     (loop [messages messages round 1 all-results [] covered #{} nudges 0]
       (let [response (llm/complete (:base-url agent-cfg) (:api-key agent-cfg)
                       (:model agent-cfg) messages tools/scribe-tool-definitions (:reasoning-effort agent-cfg))
             actions (seq (parse-scribe-calls response))]
         (if-not actions
           (do
             ;; Log every stop, not just round 1: the model declares
             ;; completion in prose after its focus list, and that content
             ;; used to vanish from the log on later rounds.
             (println (format "[scribe] LLM returned no tool calls (round %d). Content: %s"
                       round (trunc (or (:content response) "(nil)") 300)))
             (let [uncovered (vec (remove covered must-cover))]
               ;; Also respect the round cap here: a nudge must never push the
               ;; loop past max-scribe-rounds.
               (if (and (seq uncovered) (< nudges max-continue) (< round max-scribe-rounds))
                 (do
                   (println (format "[scribe] model stopped early — %d of %d files unreviewed, continuing"
                              (count uncovered) (count must-cover)))
                   (recur (conj messages
                                {:role "user"
                                 :content (format "You stopped without finishing. These files are still unreviewed: %s. Continue: read or explicitly keep each one, then reply DONE."
                                                  (clojure.string/join ", " (sort uncovered)))})
                          (inc round) all-results covered (inc nudges)))
                 all-results)))
           (let [_ (println (format "[scribe] round %d/%d: %d actions: %s"
                              round max-scribe-rounds (count actions)
                              (->> actions (map (comp name :action-type)) (clojure.string/join ", "))))
                 results (mapv #(execute-scribe-action dir cfg %) actions)
                 covered (into covered
                           (keep (fn [{:keys [action-type params]}]
                                   (case action-type
                                     :read-memory (:path params)
                                     :write-memory (:filename params)
                                     :delete-memory (:path params)
                                     nil)))
                           actions)]
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
                      (into all-results results)
                      covered
                      nudges)))))))))

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
      ;; No cap here: every memory deserves a relevance score, and
      ;; jev/chunk-batch already bounds each call — a bigger index just means
      ;; more chunks, not a bigger request.
      (let [mems (vec index)]
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

;; Curation used to skip its pass whenever the index fingerprint matched the
;; last one — but the corpus outgrew any single pass (114 files against a
;; 12-round LLM budget), so "unchanged" came to mean "never fully reviewed",
;; and trash/ meanwhile grew to 196 files that no pass ever saw. Rotation
;; replaces the fingerprint: each tick reviews exactly one topic-coherent
;; chunk and advances a persisted cursor, cycling the whole corpus every
;; chunk-count ticks regardless of churn. A pass that throws leaves the
;; cursor untouched, so the same chunk retries next tick.

(defonce ^:private curate-cursor (atom nil))

(def ^:private default-curate-chunk-size 25)
(def ^:private default-trash-retention-days 30)
(def ^:private default-sink-cap-bytes 10485760)

(defn- curate-cursor-file [cfg]
  (str (or (:state-dir cfg) "/var/lib/wayfinder") "/curate-cursor.edn"))

(defn- ensure-curate-cursor
  "Seed the rotation cursor from disk once per process: the persisted cursor
   is the chunk queue itself (vectors of path strings) plus a position into
   it, so a reboot resumes mid-cycle and — crucially — reviews exactly the
   files the tick that built the queue assigned, even though deletions in
   between have reshaped the directory. Legacy single-integer cursors
   ({:chunk n}, no :queue) and malformed files are ignored: the next tick
   simply rebuilds a fresh queue. Every failure is logged, never thrown."
  [cfg]
  (when (nil? @curate-cursor)
    (locking curate-cursor
      (when (nil? @curate-cursor)
        (try
          (let [f (File. (curate-cursor-file cfg))]
            (when (.exists f)
              (let [state (clojure.edn/read-string (slurp f))]
                (if (and (map? state) (vector? (:queue state)) (int? (:pos state)))
                  (do (reset! curate-cursor state)
                      (println (format "[scribe] curate cursor resumed: queue of %d chunks at pos %d (%s)"
                                 (count (:queue state)) (:pos state) (:at state))))
                  (println "[scribe] curate-cursor.edn has no usable queue — rebuilding from scan")))))
          (catch Exception e
            (println (format "[scribe] curate cursor unreadable — rebuilding from scan (%s)"
                       (.getMessage e)))))))))

(defn- save-curate-cursor
  "Persist the cursor (queue + position) after a successful pass. Failure to
   write is logged, not thrown: worst case a restart rebuilds a fresh queue
   from the live scan and some files get a second look sooner."
  [cfg cursor-state]
  (try
    (let [f (File. (curate-cursor-file cfg))]
      (.mkdirs (.getParentFile f))
      (spit f (pr-str cursor-state)))
    (catch Exception e
      (println (format "[scribe] curate cursor save failed: %s" (.getMessage e))))))

(defn- top-dir
  "Top-level directory of a memory path — the topic bucket for chunking.
   Root-level files (no slash) form their own group, labelled \"\"."
  [path]
  (let [path (str path)]
    (if-let [slash (clojure.string/index-of path "/")]
      (subs path 0 slash)
      "")))

(defn- curate-chunks
  "Partition the index into topic-coherent chunks: files grouped by top-level
   directory (so a colony's files land in one batch and the model can merge
   across them), groups ordered alphabetically, then packed greedily — small
   groups (one-file dirs like fiction/ or note/) share a chunk instead of each
   costing a whole tick. A group larger than chunk-size still gets its own
   sequential chunks, so big topics are never split mid-group, and the stable
   ordering is what makes the persisted cursor meaningful across ticks."
  [index chunk-size]
  (->> index
       (group-by #(top-dir (:path %)))
       (sort-by key)
       (reduce (fn [chunks [_ files]]
                 (if (> (count files) chunk-size)
                   (into chunks (partition-all chunk-size files))
                   ;; pack small groups into the trailing chunk when they fit
                   (if-let [last-chunk (peek chunks)]
                     (if (<= (+ (count last-chunk) (count files)) chunk-size)
                       (into (pop chunks) [(into last-chunk files)])
                       (conj chunks (vec files)))
                     [(vec files)])))
               [])
       vec))

(defn- purge-stale-trash
  "Deterministic hygiene, no LLM involved: delete-memory moves files into
   trash/, where they used to sit forever — invisible to scan-index and so
   to every curation pass. Files under trash/ older than the retention
   window (embedding sidecars live beside them and age with them) are
   deleted for good. Best-effort: failures are logged, never thrown."
  [dir cfg]
  (let [trash-dir (File. dir "trash")
        cutoff (- (System/currentTimeMillis)
                  (* (long (or (:trash-retention-days cfg) default-trash-retention-days))
                     24 60 60 1000))]
    (when (.isDirectory trash-dir)
      (let [stale (->> (file-seq trash-dir)
                       (filter #(.isFile %))
                       (filter #(<= (.lastModified %) cutoff))
                       vec)]
        (doseq [f stale]
          (try
            (io/delete-file f)
            (catch Exception e
              (println (format "[scribe] trash purge failed for %s: %s"
                         (.getPath f) (.getMessage e))))))
        (when (seq stale)
          (println (format "[scribe] PURGED %d stale trash files" (count stale))))))))

(defn- enforce-sink-cap
  "archive/ and trash/ are both append-only: the compactor dumps verbatim
   items into archive/ and delete-memory files (plus embedding sidecars)
   into trash/, and nothing ever shrank either, so both sinks grow forever.
   Daily tar.gz backups of the live folder exclude both sinks, so their
   contents have no recovery value — cap the pair COMBINED and evict the
   globally oldest files first. Sidecars count as independent files in the
   eviction pool, but an evicted trash/ .md also drags its .json sidecar
   along even when the sidecar itself is newer (an off-by-a-few-KB total is
   fine; the walk is idempotent). Best-effort: per-file failures are logged,
   never thrown."
  [dir cfg]
  (try
    ;; normalize: curate passes a String, but a File must work too (File.
    ;; has no one-arg ctor taking a File)
    (let [dir (io/file dir)
          cap (or (:sink-cap-bytes cfg) default-sink-cap-bytes)
          base (.toPath dir)
          files (->> ["trash" "archive"]
                     (keep #(let [d (File. dir %)] (when (.isDirectory d) d)))
                     (mapcat file-seq)
                     (filter #(.isFile %))
                     (sort-by (juxt #(.lastModified %) #(.getPath %)))
                     vec)
          total (reduce + 0 (map #(.length %) files))]
      ;; Oldest first, globally across both sinks: pop files until the
      ;; combined byte total drops under the cap.
      (loop [[f & more] files, gone #{}, total total, evicted 0, freed 0]
        (if (and (> total cap) f)
          (if (contains? gone f)
            (recur more gone total evicted freed)
            (let [rel (.toString (.relativize base (.toPath f)))
                  sidecar (when (and (.startsWith rel "trash/") (.endsWith rel ".md"))
                            (File. dir (sidecar-path rel)))
                  sidecar-live (and sidecar (.exists sidecar) (not (contains? gone sidecar)))
                  delete! (fn [f]
                            (try
                              (io/delete-file f)
                              true
                              (catch Exception e
                                (println (format "[scribe] sink-cap eviction failed for %s: %s"
                                           (.getPath f) (.getMessage e)))
                                false)))
                  ;; sizes are read BEFORE the deletes: a deleted File
                  ;; reports a length of 0
                  flen (.length f)
                  ok (delete! f)
                  evicted' (if ok (inc evicted) evicted)
                  freed' (if ok (+ freed flen) freed)
                  total' (if ok (- total flen) total)
                  slen (when sidecar-live (.length sidecar))]
              (if (and ok slen)
                (if (delete! sidecar)
                  (recur more (into gone [f sidecar])
                         (- total' slen) (inc evicted') (+ freed' slen))
                  (recur more (conj gone f) total' evicted' freed'))
                (recur more (conj gone f) total' evicted' freed'))))
          (do
            (when (pos? evicted)
              (println (format "[scribe] SINK CAP: evicted %d files (%d KB) from archive/trash (cap %d MB)"
                         evicted (quot freed 1024) (quot cap 1048576))))
            [evicted freed]))))
    (catch Exception e
      (println (format "[scribe] sink cap skipped: %s" (.getMessage e))))))

(defn- write-daily-backup
  "The sink cap throws away the only copy of evicted trash/archive content,
   so the live memory folder needs its own recovery path: one tar.gz per
   day, excluding both sinks (they are the things being capped), with
   :backup-retention-days of history kept alongside. Idempotent per day —
   curate ticks run every 30 minutes and the filename carries the date, so
   the first tick of a day writes it and every later tick returns early.
   Best-effort: failures are logged, never thrown."
  [dir cfg]
  (try
    (let [dir (io/file dir)
          backups-dir (io/file
                       (or (:backups-dir cfg)
                           (str (or (:state-dir cfg) "/var/lib/wayfinder") "/memory-backups")))
          dst (File. backups-dir (str "memory-" (java.time.LocalDate/now) ".tar.gz"))]
      (.mkdirs backups-dir)
      (if (.exists dst)
        dst
        (let [parent (.getParentFile dir)
              ;; .json sidecars are excluded too: embeddings are
              ;; near-deterministic recomputations of the content and the
              ;; sidecar summary is just the file's own first line, so
              ;; archiving 4.2MB of float arrays that regenerate on write
              ;; is waste. A restored file regenerates its sidecar on its
              ;; next write and recall degrades gracefully (Jev-only
              ;; scoring) until then. The pattern contains no slash, so
              ;; GNU tar matches it at any depth.
              result (sh/sh "tar" "-czf" (.getPath dst) "-C" (.getPath parent)
                            "--exclude=memory/archive" "--exclude=memory/trash"
                            "--exclude=*.json" "memory")]
          (if (zero? (:exit result))
            (do
              (println (format "[scribe] BACKUP wrote %s (%d KB)"
                         (.getPath dst) (quot (.length dst) 1024)))
              ;; Keep only the newest :backup-retention-days archives; the
              ;; date in each filename sorts exactly like its mtime.
              (let [keep (max 1 (or (:backup-retention-days cfg) 30))
                    archives (->> (file-seq backups-dir)
                                  (filter #(.isFile %))
                                  (sort-by #(.getName %))
                                  reverse)
                    stale (drop keep archives)]
                (doseq [f stale]
                  (try
                    (io/delete-file f)
                    (catch Exception e
                      (println (format "[scribe] backup prune failed for %s: %s"
                                 (.getPath f) (.getMessage e))))))
                (when (seq stale)
                  (println (format "[scribe] BACKUP pruned %d old archives" (count stale)))))
              dst)
            (do (println (format "[scribe] BACKUP failed: %s" (trunc (:err result) 200)))
                nil)))))
    (catch Exception e
      (println (format "[scribe] BACKUP failed: %s" (trunc (.getMessage e) 200)))
      nil)))

(defn- jev-curation-verdicts
  "One Jev choice per memory file — keep, delete, or rewrite — judged from
   each file's summary plus a short content preview. Returns
   {path {:verdict :keep|:delete|:rewrite :confidence n}} or nil when Jev is
   unavailable or no chunk answered; nil means no opinion at all: no
   deterministic deletes, and the whole batch goes to the LLM scribe
   hintless. These verdicts gate nothing — they are hints for the LLM pass,
   except :delete, which is safe to execute deterministically here because
   delete-memory-file only moves files to trash/."
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
  "The curation prompt, chunk-shaped: today's guidelines plus (a) a compact
   full-corpus map, so the model can spot cross-chunk overlap worth merging,
   (b) the assigned chunk with per-file Jev hints, and (c) an explicit DONE
   contract — the model used to declare completion in prose after its focus
   list and quit with most of the assignment unread. Verdicts are hints,
   never filters: keep, rewrite, below-threshold and no-opinion files all
   reach the LLM. Entries flagged :handled (deleted by Jev this pass) are
   listed only so the model knows where those files went."
  [index entries chunk-idx chunk-count]
  (let [corpus-str (->> index
                        (map #(str (:path %) " — " (trunc (:summary %) 60)))
                        (clojure.string/join "\n"))
        chunk-str (->> entries
                       (map #(str (:path %) (when-let [hint (:hint %)] (str " " hint))))
                       (clojure.string/join "\n"))]
    [{:role "system"
      :content (str "You are the Scribe performing memory curation. Your job is to review the stored memories assigned to you this pass and clean them up.\n\n"
                    "Guidelines:\n"
                    "- List all memories first, then read any you need to examine.\n"
                    "- MERGE: If two or more files cover the same topic or contain overlapping information, write a single consolidated file and delete the originals.\n"
                    "- DELETE: Remove files that contain no useful information (empty, trivial, redundant, or purely conversational with no factual content).\n"
                    "- PRUNE: If a file contains stale or outdated information alongside useful info, rewrite it with only current content and delete the old version.\n"
                    "- QUALITY: Every file should have a clear one-line summary as its first line. Rewrite files that lack this.\n"
                    "- Do NOT delete without reading. Do NOT merge without understanding the content.\n"
                    "- Be thorough but conservative. When in doubt, keep.\n\n"
                    "Full memory index (" (count index) " files) — you are reviewing one chunk this pass, but the full map is here so you can spot overlapping content that belongs in your chunk:\n"
                    (or corpus-str "No memories stored")
                    (format "\n\nYou are reviewing CHUNK %d/%d:\n%s\n\n"
                            (inc chunk-idx) chunk-count chunk-str)
                    "For EVERY file in the chunk that is still present: read it, then either improve it (write-memory, merging overlapping content from other chunk files), delete it (delete-memory, only if worthless), or explicitly keep it (say so in prose). Files marked [Jev: delete-executed] have already been removed — leave them alone. Do not stop until every file still present in the chunk has been read. When all are handled, reply DONE.")}
     {:role "user"
      :content "Review your assigned chunk now."}]))

;; Reference repair bounds: .md files past the char limit are skipped
;; wholesale (scanning huge notes for substrings is not worth a repair
;; sweep), and the appends per tick are capped so one heavy curation pass
;; cannot spend the whole tick rewriting referrers.
(def ^:private refpair-scan-char-limit 50000)
(def ^:private refpair-max-repairs 20)

(defn- repair-dangling-refs
  "Merges must not leave the memory graph lying: every note that pointed at
   a file this pass deleted now dangles. For each deleted path, every live
   .md file still mentioning it gets an append-only tombstone line —
   preserving the audit trail without rewriting someone's prose, and keeping
   later recall from resurrecting dead links. Deterministic and fail-open:
   one bad file logs and moves on, never throws."
  [dir cfg jev-deleted results]
  (let [;; Jev deletes already executed above; the scribe's own delete-memory
        ;; tool calls during the LLM pass may have removed more.
        deleted (->> (concat jev-deleted
                             (keep (fn [{:keys [action]}]
                                     (when (= :delete-memory (:action-type action))
                                       (get-in action [:params :path])))
                                   results))
                     (map str)
                     distinct
                     sort)
        live-md (->> (scan-index dir)
                     (filter (fn [{:keys [path]}] (.endsWith path ".md")))
                     vec)
        live-paths (set (map :path live-md))
        ;; Only truly absent paths get tombstones: a delete that failed, or a
        ;; file the scribe re-created under the same name this very pass,
        ;; leaves nothing dangling.
        dangling (vec (remove live-paths deleted))
        ;; Snapshot each surviving file once: detection runs against the
        ;; pre-repair text, so a tombstone appended for one path can never
        ;; make its own file look like a fresh referrer of another one.
        contents (into {}
                       (keep (fn [{:keys [path]}]
                               (let [f (File. dir path)]
                                 (when (.exists f)
                                   (let [text (slurp f)]
                                     (when (<= (count text) refpair-scan-char-limit)
                                       [path text]))))))
                       live-md)
        refs (sort (for [p dangling
                         [referrer text] contents
                         :when (and (not= referrer p)
                                    (clojure.string/includes? text p))]
                     [referrer p]))]
    (when (seq refs)
      (let [repairs (take refpair-max-repairs refs)
            deferred (- (count refs) (count repairs))
            date-stamp (str (java.time.LocalDate/now))]
        (when (pos? deferred)
          (println (format "[scribe] REFPAIR: %d dangling references found — repairing %d, %d left for later ticks"
                     (count refs) (count repairs) deferred)))
        (doseq [[referrer p] repairs]
          (try
            (let [f (File. dir referrer)]
              ;; write-memory-file re-embeds the appended file — fine, and
              ;; desired: its sidecar should reflect the tombstone too. This
              ;; all runs inside scribe-io-lock, so no pass can interleave.
              (write-memory-file dir referrer
                                 (str (slurp f)
                                      (format "\n\n> [curation %s] the file this note referenced (`%s`) was merged or deleted during curation; its surviving content lives in the topical files of this directory."
                                              date-stamp p))
                                 cfg))
            (println (format "[scribe] REFPAIR: %s → %s" referrer p))
            (catch Exception e
              (println (format "[scribe] REFPAIR failed for %s → %s: %s"
                         referrer p (.getMessage e))))))))))

(defn curate
  "One rotation tick: run the hygiene step (purge stale trash, cap the
   archive/trash sinks, snapshot the daily backup), then review exactly one
   topic-coherent chunk — Jev's delete verdicts execute deterministically
   (files move to trash/, nothing is lost) and every other verdict is only a
   hint for the LLM pass. The persisted cursor stores the chunk queue itself,
   not merely an index into a recomputed layout: deletions during a cycle
   reshape any freshly derived layout, so a re-derived index would silently
   point at the wrong files. Each successful tick advances the position by
   one queue slot, so full coverage arrives in chunk-count ticks without any
   single pass having to swallow the whole corpus."
  [cfg]
  (locking scribe-io-lock
    (ensure-curate-cursor cfg)
    (let [dir (ensure-dir (memory-dir cfg))
          chunk-size (max 1 (or (:curate-chunk-size cfg) default-curate-chunk-size))]
      ;; Purge before the pass: pure file hygiene, and a failure here should
      ;; not cost the chunk its LLM budget.
      (try (purge-stale-trash dir cfg)
           (catch Exception e
             (println (format "[scribe] trash purge skipped: %s" (.getMessage e)))))
      ;; Cap and backup follow the purge, still before the pass: evictions
      ;; leave the live folder in its pre-pass shape, and the backup runs
      ;; last so it captures that cleaned state. Both are fail-open on their
      ;; own; the wraps just keep a hygiene hiccup from costing the chunk
      ;; its LLM budget, same as the purge above.
      (try (enforce-sink-cap dir cfg)
           (catch Exception e
             (println (format "[scribe] sink cap skipped: %s" (.getMessage e)))))
      (try (write-daily-backup dir cfg)
           (catch Exception e
             (println (format "[scribe] daily backup skipped: %s" (.getMessage e)))))
      (let [index (scan-index dir)]
        (if (empty? index)
          (do (println "[scribe] CURATE: memory index empty — nothing to curate")
              [])
          (let [cursor @curate-cursor
                queued (:queue cursor)
                cursor-pos (:pos cursor)
                ;; The queue IS the cursor: while the persisted queue still
                ;; has an unvisited slot, that slot names this tick's files
                ;; exactly as the tick that built it assigned them — a
                ;; deletion mid-cycle must not reshape the layout under our
                ;; feet. Only a missing, legacy or exhausted cursor derives
                ;; a fresh layout, stores the whole queue (vectors of path
                ;; strings) and starts at pos 0.
                [queue cursor-pos]
                (if (and (vector? queued) (int? cursor-pos) (< cursor-pos (count queued)))
                  [queued cursor-pos]
                  (let [q (mapv #(mapv :path %) (curate-chunks index chunk-size))]
                    ;; Stash the fresh queue in memory right away: even if
                    ;; this tick dies before the save below, the next one
                    ;; resumes this layout instead of reshaping it.
                    (reset! curate-cursor {:queue q :pos 0 :at (str (java.time.Instant/now))})
                    [q 0]))
                chunk-count (count queue)
                chunk-paths (nth queue cursor-pos)
                ;; Resolve at execution time: paths deleted since the queue
                ;; was built (hygiene here, or curation in an earlier tick)
                ;; are simply skipped — coverage means every surviving path
                ;; appears in exactly one queue slot.
                by-path (into {} (map (fn [{:keys [path] :as entry}] [path entry])) index)
                chunk (->> chunk-paths (keep by-path) vec)
                gone (- (count chunk-paths) (count chunk))
                _ (when (pos? gone)
                    (println (format "[scribe] chunk %d: %d of %d paths already gone"
                              (inc cursor-pos) gone (count chunk-paths))))
                ;; Jev grades only this chunk; its per-call chunking (60
                ;; questions) is inherited from jev-curation-verdicts itself.
                verdicts (jev-curation-verdicts cfg dir chunk)
                deletes (into {} (filter (fn [[_ v]] (= :delete (:verdict v))) verdicts))
                rewrites (count (filter (fn [[_ v]] (= :rewrite (:verdict v))) verdicts))]
            (println (format "[scribe] CURATE chunk %d/%d: %d files (%s) — Jev: %d deletes, %d rewrites flagged"
                       (inc cursor-pos) chunk-count (count chunk)
                       (->> chunk
                            (map #(let [d (top-dir (:path %))] (if (= d "") "root" d)))
                            distinct sort
                            (clojure.string/join ", "))
                       (count deletes) rewrites))
            ;; Deletes are safe to execute deterministically:
            ;; delete-memory-file moves files to trash/, nothing is lost.
            (doseq [[path {:keys [confidence]}] deletes]
              (println (format "[scribe] CURATE DELETE %s (Jev, confidence %.2f)" path (double confidence)))
              (delete-memory-file dir path))
            ;; Re-scan AFTER the deletes: the prompt used to list files that
            ;; were already gone.
            (let [live-paths (set (map :path (scan-index dir)))
                  entries (map (fn [{:keys [path summary]}]
                                 (if (contains? live-paths path)
                                   {:path path :summary summary
                                    :hint (when-let [{:keys [verdict confidence]} (get verdicts path)]
                                            (when-not (= :delete verdict)
                                              (format "[Jev: %s %.2f]" (name verdict) (double confidence))))}
                                   {:path path :summary summary
                                    :hint "[Jev: delete-executed]" :handled? true}))
                               chunk)
                  assigned (vec (remove :handled? entries))
                  results (if (seq assigned)
                            (run-scribe-turn cfg dir
                                             (curation-messages index entries cursor-pos chunk-count)
                                             {:must-cover (map :path assigned)})
                            (do (println (if (seq deletes)
                                           "[scribe] CURATE: Jev deleted every file in the chunk — LLM pass skipped"
                                           "[scribe] CURATE: nothing left to review in this chunk — LLM pass skipped"))
                                []))
                  covered (->> results
                               (keep (fn [{:keys [action]}]
                                       (case (:action-type action)
                                         :read-memory (get-in action [:params :path])
                                         :write-memory (get-in action [:params :filename])
                                         :delete-memory (get-in action [:params :path])
                                         nil)))
                               set)
                  covered-n (count (filter covered (map :path assigned)))
                  next-state {:queue queue :pos (inc cursor-pos) :at (str (java.time.Instant/now))}]
              (reset! curate-cursor next-state)
              (save-curate-cursor cfg next-state)
              (println (format "[scribe] CURATE chunk %d done: %d/%d files covered, cursor → %d"
                         (inc cursor-pos) covered-n (count assigned) (:pos next-state)))
              ;; Merges must not leave the memory graph lying: everything
              ;; this pass deleted (Jev + scribe tool calls) gets its
              ;; referrers tombstoned, append-only (fail-open; capped).
              (try (repair-dangling-refs dir cfg (keys deletes) results)
                   (catch Exception e
                     (println (format "[scribe] refpair pass skipped: %s" (.getMessage e)))))
              results)))))))
