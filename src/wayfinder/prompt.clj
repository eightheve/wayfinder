(ns wayfinder.prompt
  (:require [wayfinder.context :as context]
            [cheshire.core :as json]))

(defmulti render-item
  (fn [item] (if (= :summarized (:salience item)) :summarized (:type item))))

(defmethod render-item :summarized [item]
  {:role "user"
   :content (:content (:data item))})

(defmethod render-item :system-prompt [item]
  {:role "system"
   :content (:content (:data item))})

(defmethod render-item :notification [item]
  {:role "user"
   :content (:content (:data item))})

(defmethod render-item :user-message [item]
  {:role "user"
   :content (:content (:data item))})

(defmethod render-item :reasoning [item]
  {:role "assistant"
   :content (:content (:data item))})

(defmethod render-item :action [item]
  (let [{:keys [action-type params]} (:data item)]
    {:role "assistant"
     :content nil
     :tool_calls [{:id (str "call_" (:id item))
                   :type "function"
                   :function {:name (name action-type)
                              :arguments (json/generate-string params)}}]}))

(defmethod render-item :action-result [item]
  {:role "tool"
   :tool_call_id (str "call_" (:caused-by (:data item)))
   :content (:content (:data item))})

(defmethod render-item :memory [item]
  {:role "user"
   :content (str "Long-term memory recall: " (:content (:data item)))})

(defmethod render-item :default [item]
  {:role "user"
   :content (pr-str (:data item))})

(defn assemble [ctx]
  (->> (context/fetch-context ctx)
       (mapv render-item)))
