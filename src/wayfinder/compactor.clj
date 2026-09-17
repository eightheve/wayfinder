(ns wayfinder.compactor
  (:require [wayfinder.context :as context]
            [wayfinder.jev :as jev]
            [wayfinder.llm :as llm]
            [wayfinder.scribe :as scribe]
            [wayfinder.tools :as tools]
            [cheshire.core :as json]
            [clojure.java.io :as io]))

(defn- trunc [s max-len]
  (let [s (str s)]
    (if (> (count s) max-len)
      (str (subs s 0 max-len) "...")
      s)))

(def ^:private low-value-results
  #{"No unread messages." "Memory recall initiated" "Memory curation initiated"
    "(duplicate result suppressed)"})

(defn- low-value? [item]
  (and (= :action-result (:type item))
       (let [content (get-in item [:data :content])]
         (contains? low-value-results content))))

(defn prune-low-value [ctx]
  (let [items (context/fetch-context @ctx)
        recent (take-last 20 items)
        recent-ids (set (map :id recent))
        older (remove #(contains? recent-ids (:id %)) items)
        ;; NB: (filter low-value?) selects the LOW-VALUE items — these are the
        ;; ones to forget. The previous destructuring had the two halves
        ;; swapped and deleted everything EXCEPT the boilerplate.
        low-value (filter low-value? older)]
    (when (seq low-value)
      (let [breakdown (->> low-value
                           (map #(get-in % [:data :content]))
                           (frequencies)
                           (map (fn [[k v]] (format "%dx \"%s\"" v (trunc k 60))))
                           (clojure.string/join ", "))]
        (println (format "[compactor] Pruned %d low-value items from context: %s"
                   (count low-value) breakdown))
        (swap! ctx context/forget-items-with-pairs (map :id low-value))))))

(defn- protected-ids [items]
  (let [recent (set (map :id (take-last 20 items)))]
    (into recent (map :id (filter :pinned items)))))

(defn- format-context-for-compaction [ctx]
  (let [items (context/fetch-context ctx)
        prot-ids (protected-ids items)]
    (->> (remove #(contains? prot-ids (:id %)) items)
         (take 100)
         (map (fn [item]
                (format "[%d] %s (%s) — %s"
                  (:id item)
                  (name (:type item))
                  (name (:salience item))
                  (trunc (pr-str (:data item)) 500))))
         (clojure.string/join "\n"))))

(defn parse-compactor-calls [response]
  (when-let [calls (:tool_calls response)]
    (for [call calls]
      (let [func (:function call)]
        {:action-type (keyword (:name func))
         :params (try (json/parse-string (:arguments func) true)
                      (catch Exception _ {}))}))))

(defn- file-to-scribe [cfg items]
  (when (seq items)
    (println (format "[compactor] Filing %d items to long-term memory" (count items)))
    (future
      (try
        (scribe/file-memories cfg items)
        (catch Exception e
          (println (str "[compactor] ERROR filing memories: " (.getMessage e))))))))

(defn- archive-verbatim! [cfg items]
  (when (seq items)
    (let [memory-dir (or (:memory-dir cfg) "/var/lib/wayfinder/memory")
          archive-dir (io/file memory-dir "archive")
          f (io/file archive-dir (str (System/currentTimeMillis) ".edn"))
          content (pr-str items)]
      (.mkdirs archive-dir)
      (spit f content)
      (assert (= (slurp f) content) "archive read-back failed")
      (println (format "[compactor] Archived %d items to %s" (count items) (.getPath f)))
      (.getPath f))))

(defn- ids-to-forget-from-action [{:keys [action-type params]}]
  (case action-type
    :summarize-item (let [ids (or (seq (:ids params)) (when-let [id (:id params)] [id]))]
                      (when (seq ids) (remove #(= (apply max ids) %) ids)))
    :forget-item (or (seq (:ids params)) (when-let [id (:id params)] [id]))
    nil))

(defn- archive-items-for-actions! [ctx cfg actions prot-ids]
  (let [all-items (context/fetch-context @ctx)
        item-map (into {} (map (juxt :id identity) all-items))
        ids-to-archive (->> (mapcat ids-to-forget-from-action actions)
                            distinct
                            (remove prot-ids))
        items-to-archive (keep item-map ids-to-archive)]
    (archive-verbatim! cfg items-to-archive)))

(defn apply-compaction [ctx cfg actions]
  (let [prot-ids (protected-ids (context/fetch-context @ctx))]
    (archive-items-for-actions! ctx cfg actions prot-ids)
    (let [forgotten (atom [])
          to-remember (atom [])
          summarized-ids (atom 0)
          summary-count (atom 0)]
      (doseq [{:keys [action-type params]} actions]
        (case action-type
          :summarize-item
          (let [raw-ids (or (seq (:ids params)) (when-let [id (:id params)] [id]))
                ids (remove prot-ids raw-ids)]
            (if (seq ids)
              (let [remember? (:remember params)
                    items (context/fetch-ids @ctx ids)]
                (when remember?
                  (swap! to-remember into items)
                  (println (format "[compactor] SUMMARIZE+REMEMBER items %s → %s"
                             (pr-str ids) (trunc (:summary params) 120))))
                (when-not remember?
                  (println (format "[compactor] SUMMARIZE items %s → %s"
                             (pr-str ids) (trunc (:summary params) 120))))
                (swap! summarized-ids + (count ids))
                (swap! summary-count inc)
                (swap! ctx context/summarize-items ids {:content (:summary params)} remember?))
              (println (format "[compactor] Skipping summarize-item — all of %s protected" (pr-str raw-ids)))))

          :forget-item
          (let [raw-ids (or (seq (:ids params)) (when-let [id (:id params)] [id]))
                ids (remove prot-ids raw-ids)]
            (if (seq ids)
              (let [items (context/fetch-ids @ctx ids)]
                (doseq [item items]
                  (if (:remembered item)
                    (println (format "[compactor] FORGET item %d (already in long-term memory, skipping scribe)" (:id item)))
                    (do
                      (swap! forgotten conj item)
                      (println (format "[compactor] FORGET item %d" (:id item))))))
                (swap! ctx context/forget-items-with-pairs ids))
              (println (format "[compactor] Skipping forget-item — all of %s protected" (pr-str raw-ids)))))

          :file-to-memory
          (let [id (:id params)
                item (first (context/fetch-id @ctx id))]
            (if item
              (let [preview (trunc (pr-str (:data item)) 120)]
                (swap! to-remember conj item)
                (swap! ctx context/update-item id {:remembered true})
                (println (format "[compactor] FILE-TO-MEMORY item %d: %s" id preview)))
              (println (format "[compactor] FILE-TO-MEMORY skipped — no item with id %s" (pr-str id)))))

          nil))
      ;; Compaction transparency (requested by the resident, audit Q3):
      ;; a compact in-context notice of what just changed, so pinning and
      ;; re-ingestion stay possible. Non-intrusive: one line, opt-out by
      ;; the compactor forgetting it later like any other item.
      (let [n-forgot (count @forgotten)
            n-filed (count @to-remember)]
        (when (or (pos? @summary-count) (pos? n-forgot) (pos? n-filed))
          (swap! ctx context/add-item :system-note
            {:content (format "Context compacted: %d items merged into %d summaries, %d forgotten, %d filed to long-term memory. Pin anything you must keep verbatim."
                        @summarized-ids @summary-count n-forgot n-filed)})))
      (file-to-scribe cfg (concat @to-remember @forgotten)))))

(defn- llm-compact-pass
  "The original generative path: the compactor LLM decides and writes. Runs
   when Jev is absent, gave no opinion, or errored."
  [ctx cfg target]
  (let [items (context/fetch-context @ctx)
        item-count (count items)
        token-count (context/token-estimate items)
        context-str (format-context-for-compaction @ctx)]
    (println (format "[compactor] Running compaction (%d items, %d tokens, target %d tokens)" item-count token-count target))
    (let [messages [{:role "system"
                     :content (str "You are a context compactor. Your job is to reduce context size by summarizing and forgetting items.\n\n"
                                   "Current context has " item-count " items / " token-count " tokens (you see up to 100 oldest after filtering). Target is " target " tokens.\n\n"
                                   "Guidelines:\n"
                                   "- Prioritize compacting OLD items first — items with lower IDs are older and more likely to be summarizable.\n"
                                   "- Do NOT summarize or forget items from the last few turns (high IDs near " item-count "). The agent needs recent context to function.\n"
                                   "- Cover MULTIPLE topics per pass. Scan the full list and act on items from different subjects — don't fixate on one cluster.\n"
                                   "- Use summarize-item with multiple IDs to merge related items into one summary. The summary replaces the newest item in the batch; all others are forgotten.\n"
                                   "- Set remember=true on summarize-item for any item containing knowledge worth retaining indefinitely (facts, configs, decisions, architecture). This files the original content to long-term memory before summarizing.\n"
                                   "- Use forget-item with multiple IDs to remove several trivial items at once.\n"
                                   "- Use file-to-memory for concise items that contain important knowledge but don't need summarizing — this marks them as remembered without changing them.\n"
                                   "- Batch your actions — use arrays of IDs wherever possible to compact efficiently.")}
                    {:role "user"
                     :content context-str}]
          agent-cfg (get-in cfg [:agents :compactor])
          response (llm/complete (:base-url agent-cfg) (:api-key agent-cfg)
                     (:model agent-cfg) messages tools/compactor-tool-definitions (:reasoning-effort agent-cfg))]
      (if-let [actions (seq (parse-compactor-calls response))]
        (do
          (println (format "[compactor] LLM returned %d actions" (count actions)))
          (apply-compaction ctx cfg actions))
        (println "[compactor] LLM returned no actions — nothing to compact")))))

;; --- Jev triage ---
;; Retention used to be three generative decisions (forget / summarize / file)
;; made one LLM-tool-call at a time. They are closed-set judgments over items
;; the judge can see side by side — exactly Jev's shape: one parallel pass,
;; calibrated confidence per item, and the LLM is only asked to do the one
;; thing Jev cannot: write the summaries.

(defn- jev-triage
  "Judge retention for all unprotected items in one (chunked) Jev call.
   Returns {item-id {:decision :forget|:summarize|:file :confidence n}}, nil
   when Jev returned no opinions at all (caller falls back to the LLM pass)."
  [cfg ctx]
  (let [items (context/fetch-context @ctx)
        prot-ids (protected-ids items)
        candidates (vec (remove #(contains? prot-ids (:id %)) items))
        threshold (or (:jev-triage-threshold cfg) 0.55)]
    (when (seq candidates)
      (let [answered (atom false)
            triaged
            (reduce into {}
              (keep
                (fn [chunk]
                  (let [state (clojure.string/join "\n"
                                (map (fn [item]
                                       (format "[%d] %s/%s — %s"
                                         (:id item) (name (:type item)) (name (:salience item))
                                         (trunc (pr-str (:data item)) 400)))
                                     chunk))
                        questions (into {}
                                    (map (fn [item]
                                           [(str "i_" (:id item))
                                            {:type "choice"
                                             ;; The question id is never shown to the model — the
                                             ;; item id must be in the instructions text itself.
                                             :instructions (str "You are the retention judge for a long-running agent's working context. Item " (:id item) " is listed in the state as '[" (:id item) "] ...'. What should happen to it?")
                                             :criteria {"forget" "No lasting value: boilerplate, receipts, greetings, superseded state, filler reasoning. Forgetting loses nothing."
                                                        "summarize" "Worth keeping as a short summary: decisions, findings, conversation substance. The raw detail can go."
                                                        "file" "Dense factual knowledge worth preserving VERBATIM in long-term memory: architecture, configs, user facts."}}])
                                         chunk))]
                    (when-let [result (jev/evaluate cfg state questions)]
                      (reset! answered true)
                      (into {}
                        (keep (fn [item]
                              (let [answer (get (:answers result) (str "i_" (:id item)))
                                    decision (some-> (:choice answer) keyword)
                                    confidence (:confidence answer)]
                                ;; Below-threshold confidence means 'keep as
                                ;; raw': uncertainty is always conservative.
                                (when (and decision confidence (>= confidence threshold))
                                  [(:id item) {:decision decision :confidence (double confidence)}])))
                            chunk)))))
                (jev/chunk-batch candidates)))]
        (println (format "[compactor] Jev triage: %d items judged"
                   (if (map? triaged) (count triaged) 0)))
        (when @answered triaged)))))

(defn- llm-summary-pass
  "The one thing Jev cannot do: write the summaries. Only items triaged as
   :summarize are shown, and the LLM's ONLY task is merging them."
  [ctx cfg ids]
  (let [items (context/fetch-ids @ctx ids)]
    (if-not (seq items)
      (println "[compactor] Summarize pass: flagged items no longer in context — skipping")
      (let [items-str (->> items
                           (map #(format "[%d] %s/%s — %s"
                                   (:id %) (name (:type %)) (name (:salience %))
                                   (trunc (pr-str (:data %)) 500)))
                           (clojure.string/join "\n"))
            messages [{:role "system"
                       :content (str "You are a context compactor. A triage pass has already judged every item below to be worth keeping as a SUMMARY — do not forget anything, do not re-judge. Your only job is to merge related items into concise summaries.\n\n"
                                     "Guidelines:\n"
                                     "- Use summarize-item with multiple IDs to merge related items into one summary. The summary replaces the newest item in the batch; all others are forgotten.\n"
                                     "- Unrelated items each get their own summarize-item call with a single ID.\n"
                                     "- Set remember=true for any item containing knowledge worth retaining indefinitely (facts, configs, decisions).\n"
                                     "- Cover every listed item.")}
                      {:role "user"
                       :content items-str}]
            agent-cfg (get-in cfg [:agents :compactor])
            response (llm/complete (:base-url agent-cfg) (:api-key agent-cfg)
                       (:model agent-cfg) messages tools/compactor-tool-definitions (:reasoning-effort agent-cfg))
            actions (parse-compactor-calls response)
            summarize-actions (filter #(= :summarize-item (:action-type %)) actions)
            to-file (atom [])]
        (println (format "[compactor] Summarize pass: %d items flagged, LLM returned %d summarize calls"
                   (count items) (count summarize-actions)))
        (doseq [{:keys [params]} summarize-actions]
          (let [raw-ids (or (seq (:ids params)) (when-let [id (:id params)] [id]))
                valid-ids (filter (set ids) raw-ids)]
            (when (seq valid-ids)
              (when (:remember params)
                (swap! to-file into (context/fetch-ids @ctx valid-ids)))
              (println (format "[compactor] SUMMARIZE (triage-flagged) items %s → %s"
                         (pr-str valid-ids) (trunc (:summary params) 120)))
              (swap! ctx context/summarize-items valid-ids {:content (:summary params)}
                     (boolean (:remember params))))))
        (file-to-scribe cfg @to-file)))))

(defn- apply-jev-triage!
  "Apply Jev's retention map. Forget is archived verbatim first, then dropped
   (pairs included); file marks items remembered and ships them to the scribe;
   summarize goes to the LLM summary pass. The decisions are logged as a
   transparency :system-note just like the LLM path's."
  [ctx cfg triaged]
  (let [items (context/fetch-context @ctx)
        item-map (into {} (map (juxt :id identity) items))
        by-decision (group-by (comp :decision val) triaged)
        forget-ids (vec (map key (:forget by-decision)))
        file-ids (vec (map key (:file by-decision)))
        summarize-ids (vec (map key (:summarize by-decision)))
        forget-items (keep item-map forget-ids)
        file-items (keep item-map file-ids)]
    (archive-verbatim! cfg forget-items)
    (when (seq forget-ids)
      (println (format "[compactor] Jev FORGET items %s" (pr-str forget-ids)))
      (swap! ctx context/forget-items-with-pairs forget-ids))
    (doseq [{item-id :id :as item} file-items]
      (swap! ctx context/update-item item-id {:remembered true})
      (println (format "[compactor] Jev FILE-TO-MEMORY item %d: %s"
                 item-id (trunc (pr-str (:data item)) 120))))
    ;; Already-remembered items were filed when summarized; forgetting them
    ;; later must not re-persist duplicates (same rule as apply-compaction).
    (file-to-scribe cfg (concat file-items (remove :remembered forget-items)))
    (when (or (seq forget-ids) (seq file-ids) (seq summarize-ids))
      (swap! ctx context/add-item :system-note
        {:content (format "Context compacted by Jev triage: %d items forgotten, %d filed to long-term memory, %d flagged for summarization. Pin anything you must keep verbatim."
                    (count forget-ids) (count file-ids) (count summarize-ids))}))
    (when (seq summarize-ids)
      (llm-summary-pass ctx cfg summarize-ids))))

(defn compact [ctx cfg target]
  (prune-low-value ctx)
  (let [triaged (when (jev/available? cfg)
                  (try
                    (jev-triage cfg ctx)
                    (catch Exception e
                      (println (format "[compactor] Jev triage failed (%s) — falling back to LLM pass"
                                 (ex-message e)))
                      nil)))]
    ;; nil means Jev had nothing to say — the LLM path stays the fallback.
    ;; An empty map means Jev judged everything 'keep' and the pass is over.
    (if (some? triaged)
      (apply-jev-triage! ctx cfg triaged)
      (llm-compact-pass ctx cfg target))))
