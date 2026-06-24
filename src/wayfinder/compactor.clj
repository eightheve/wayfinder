(ns wayfinder.compactor
  (:require [wayfinder.context :as context]
            [wayfinder.llm :as llm]
            [wayfinder.scribe :as scribe]
            [wayfinder.tools :as tools]
            [cheshire.core :as json]))

(defn format-context-for-compaction [ctx]
  (->> (context/fetch-context ctx)
       (map (fn [item]
              (format "[%d] %s (%s) — %s"
                (:id item)
                (name (:type item))
                (name (:salience item))
                (pr-str (:data item)))))
       (clojure.string/join "\n")))

(defn parse-compactor-calls [response]
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

(defn- file-to-scribe [cfg items]
  (when (seq items)
    (println (format "[compactor] Filing %d items to long-term memory" (count items)))
    (future
      (try
        (scribe/file-memories cfg items)
        (catch Exception e
          (println (str "[compactor] ERROR filing memories: " (.getMessage e))))))))

(defn apply-compaction [ctx cfg actions]
  (let [forgotten (atom [])
        to-remember (atom [])]
    (doseq [{:keys [action-type params]} actions]
      (case action-type
        :summarize-item
        (let [id (:id params)
              remember? (:remember params)
              item (first (context/fetch-id @ctx id))
              old-preview (trunc (pr-str (:data item)) 120)]
          (when remember?
            (swap! to-remember conj item)
            (println (format "[compactor] SUMMARIZE+REMEMBER item %d: %s → %s"
                       id old-preview (trunc (:summary params) 120))))
          (when-not remember?
            (println (format "[compactor] SUMMARIZE item %d: %s → %s"
                       id old-preview (trunc (:summary params) 120))))
          (swap! ctx context/summarize-item id {:content (:summary params)} remember?))
        :forget-item
        (let [id (:id params)
              item (first (context/fetch-id @ctx id))
              preview (trunc (pr-str (:data item)) 120)]
          (if (:remembered item)
            (println (format "[compactor] FORGET item %d: %s (already in long-term memory, skipping scribe)" id preview))
            (swap! forgotten conj item))
          (swap! ctx context/forget-item id)
          (when-not (:remembered item)
            (println (format "[compactor] FORGET item %d: %s" id preview))))
        :file-to-memory
        (let [id (:id params)
              item (first (context/fetch-id @ctx id))
              preview (trunc (pr-str (:data item)) 120)]
          (swap! to-remember conj item)
          (swap! ctx context/update-item id {:remembered true})
          (println (format "[compactor] FILE-TO-MEMORY item %d: %s" id preview)))
        nil))
    (file-to-scribe cfg (concat @to-remember @forgotten))))

(defn compact [ctx cfg]
  (let [item-count (count (:items @ctx))
        context-str (format-context-for-compaction @ctx)]
    (println (format "[compactor] Running compaction (%d items in context)" item-count))
    (let [messages [{:role "system"
                     :content (str "You are a context compactor. Your job is to reduce context size by summarizing and forgetting items.\n\n"
                                   "Guidelines:\n"
                                   "- Use summarize-item liberally to compress large or verbose items into concise summaries.\n"
                                   "- Set remember=true on summarize-item for any item containing knowledge worth retaining indefinitely (facts, configs, decisions, architecture). This files the original content to long-term memory before summarizing.\n"
                                   "- Use forget-item only for truly trivial details (greetings, acknowledgments, \"message sent\" confirmations) or items so old they're irrelevant.\n"
                                   "- Use file-to-memory for concise items that contain important knowledge but don't need summarizing — this marks them as remembered without changing them.\n"
                                   "- The agent needs its context to function: over-forgetting will cripple it. When in doubt, summarize instead of forget.\n"
                                   "- NEVER touch item id 0, the system prompt.\n"
                                   "- Batch your actions — issue multiple tool calls in a single response to compact efficiently.")}
                    {:role "user"
                     :content context-str}]
          response (llm/complete (:base-url cfg) (:api-key cfg)
                     (get-in cfg [:models :small]) messages tools/compactor-tool-definitions)]
      (if-let [actions (seq (parse-compactor-calls response))]
        (do
          (println (format "[compactor] LLM returned %d actions" (count actions)))
          (apply-compaction ctx cfg actions))
        (println "[compactor] LLM returned no actions — nothing to compact")))))
