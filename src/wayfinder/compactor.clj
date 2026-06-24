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

(defn apply-compaction [ctx cfg actions]
  (let [forgotten (atom [])]
    (doseq [{:keys [action-type params]} actions]
      (case action-type
        :summarize-item
        (let [id (:id params)
              item (first (context/fetch-id @ctx id))
              old-preview (trunc (pr-str (:data item)) 120)]
          (swap! ctx context/summarize-item id {:content (:summary params)})
          (println (format "[compactor] SUMMARIZE item %d: %s → %s"
                     id old-preview (trunc (:summary params) 120))))
        :forget-item
        (let [id (:id params)
              item (first (context/fetch-id @ctx id))
              preview (trunc (pr-str (:data item)) 120)]
          (swap! forgotten conj item)
          (swap! ctx context/forget-item id)
          (println (format "[compactor] FORGET item %d: %s" id preview)))
        nil))
    (when (seq @forgotten)
      (println (format "[compactor] Filing %d forgotten items to long-term memory" (count @forgotten)))
      (future
        (try
          (scribe/file-memories cfg @forgotten)
          (catch Exception e
            (println (str "[compactor] ERROR filing memories: " (.getMessage e)))))))))

(defn compact [ctx cfg]
  (let [item-count (count (:items @ctx))
        context-str (format-context-for-compaction @ctx)]
    (println (format "[compactor] Running compaction (%d items in context)" item-count))
    (let [messages [{:role "system"
                     :content "You are a context compactor. Your primary tool is summarize-item — use it liberally to compress large or verbose items into concise summaries. Only use forget-item as a last resort for truly trivial details (greetings, acknowledgments) or items so old they are no longer relevant at all. The agent needs its context to function: over-forgetting will cripple it. When in doubt, summarize instead of forget. NEVER touch item id 0, the system prompt."}
                    {:role "user"
                     :content context-str}]
          response (llm/complete (:base-url cfg) (:api-key cfg)
                     (get-in cfg [:models :small]) messages tools/compactor-tool-definitions)]
      (if-let [actions (seq (parse-compactor-calls response))]
        (do
          (println (format "[compactor] LLM returned %d actions" (count actions)))
          (apply-compaction ctx cfg actions))
        (println "[compactor] LLM returned no actions — nothing to compact")))))
