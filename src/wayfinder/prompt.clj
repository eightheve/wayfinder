(ns wayfinder.prompt
  (:require [wayfinder.context :as context]
            [cheshire.core :as json]))

(defmulti render-item
  (fn [item] (if (= :summarized (:salience item)) :summarized (:type item))))

(defmethod render-item :summarized [item]
  {:role "user"
   :content (str "[context summary] " (:content (:data item)))})

(defmethod render-item :notification [item]
  {:role "user"
   :content (str "[notification] " (:content (:data item)))})

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
   :content (str "[memory recall] " (:content (:data item)))})

(defmethod render-item :default [item]
  {:role "user"
   :content (pr-str (:data item))})

(defn- idle-patterns [ctx]
  (let [tail (take-last 10 (:items ctx))]
    (->> tail
         (filter (fn [item]
                   (or (and (= :action (:type item))
                            (let [atype (get-in item [:data :action-type])]
                              (#{:wait :check-messages} atype)))
                       (and (= :action-result (:type item))
                            (re-find #"(?i)no unread|no messages|nothing to"
                              (str (get-in item [:data :content])))))))
         count)))

(defn- nudge-for [idle-count]
  (cond
    (<= 3 idle-count 5)
    {:role "system"
     :content "You've been idle for a while. You feel restless and want to investigate or learn something."}
    (<= 6 idle-count 8)
    {:role "system"
     :content "You've been idle too long. You're uncomfortable doing nothing — go explore something, figure something out, follow up on an observation."}
    (>= idle-count 9)
    {:role "system"
     :content "Extended idleness. You need to do something. Find something to understand, investigate, or create."}
    :else nil))

(defn assemble [ctx system-prompt]
  (let [items (->> (context/fetch-context ctx)
                   (mapv render-item))
        nudge (nudge-for (idle-patterns ctx))]
    (cond-> [{:role "system" :content system-prompt}]
      (seq items) (into items)
      nudge (conj nudge))))
