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

(defn apply-compaction [ctx cfg actions]
  (let [forgotten (atom [])]
    (doseq [{:keys [action-type params]} actions]
      (case action-type
        :summarize-item (swap! ctx context/summarize-item (:id params) {:content (:summary params)})
        :forget-item (let [item (first (context/fetch-id @ctx (:id params)))]
                       (swap! forgotten conj item)
                       (swap! ctx context/forget-item (:id params)))
        nil))
    (when (seq @forgotten)
      (future (scribe/file-memories cfg @forgotten)))))

(defn compact [ctx cfg]
  (let [context-str (format-context-for-compaction @ctx)
        messages [{:role "system"
                   :content "You are a context compactor. Your primary tool is summarize-item — use it liberally to compress large or verbose items into concise summaries. Only use forget-item as a last resort for truly trivial details (greetings, acknowledgments) or items so old they are no longer relevant at all. The agent needs its context to function: over-forgetting will cripple it. When in doubt, summarize instead of forget. NEVER touch item id 0, the system prompt."}
                  {:role "user"
                   :content context-str}]
        response (llm/complete (:base-url cfg) (:api-key cfg)
                   (get-in cfg [:models :small]) messages tools/compactor-tool-definitions)]
    (when-let [actions (seq (parse-compactor-calls response))]
      (apply-compaction ctx cfg actions))))
