(ns wayfinder.compactor
  (:require [wayfinder.context :as context]
            [wayfinder.llm :as llm]
            [wayfinder.scribe :as scribe]
            [wayfinder.tools :as tools]
            [cheshire.core :as json]))

(defn- trunc [s max-len]
  (let [s (str s)]
    (if (> (count s) max-len)
      (str (subs s 0 max-len) "...")
      s)))

(defn format-context-for-compaction [ctx]
  (->> (context/fetch-context ctx)
       (take 100)
       (map (fn [item]
              (format "[%d] %s (%s) — %s"
                (:id item)
                (name (:type item))
                (name (:salience item))
                (trunc (pr-str (:data item)) 500))))
       (clojure.string/join "\n")))

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

(defn compact [ctx cfg target]
  (let [item-count (count (context/fetch-context @ctx))
        context-str (format-context-for-compaction @ctx)]
    (println (format "[compactor] Running compaction (%d items, target %d)" item-count target))
    (let [messages [{:role "system"
                     :content (str "You are a context compactor. Your job is to reduce context size by summarizing and forgetting items.\n\n"
                                   "Current context has " item-count " items (you see the 100 oldest). Target is " target " items.\n\n"
                                   "Guidelines:\n"
                                   "- Prioritize compacting OLD items first — items with lower IDs are older and more likely to be summarizable.\n"
                                   "- Do NOT summarize or forget items from the last few turns (high IDs near " item-count "). The agent needs recent context to function.\n"
                                   "- Use summarize-item liberally to compress large or verbose items into concise summaries.\n"
                                   "- Set remember=true on summarize-item for any item containing knowledge worth retaining indefinitely (facts, configs, decisions, architecture). This files the original content to long-term memory before summarizing.\n"
                                   "- Use forget-item only for truly trivial details (greetings, acknowledgments, \"message sent\" confirmations) or items so old they're irrelevant.\n"
                                   "- Use file-to-memory for concise items that contain important knowledge but don't need summarizing — this marks them as remembered without changing them.\n"
                                   "- Batch your actions — issue multiple tool calls in a single response to compact efficiently.")}
                    {:role "user"
                     :content context-str}]
          agent-cfg (get-in cfg [:agents :compactor])
          response (llm/complete (:base-url cfg) (:api-key cfg)
                     (:model agent-cfg) messages tools/compactor-tool-definitions (:reasoning-effort agent-cfg))]
      (if-let [actions (seq (parse-compactor-calls response))]
        (do
          (println (format "[compactor] LLM returned %d actions" (count actions)))
          (apply-compaction ctx cfg actions))
        (println "[compactor] LLM returned no actions — nothing to compact")))))
