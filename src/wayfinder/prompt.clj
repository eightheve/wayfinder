(ns wayfinder.prompt
  (:require [wayfinder.context :as context]
            [cheshire.core :as json]))

;; NB: send-message actions used to render as plain assistant text
;; (:sent-message special case). They now render as ordinary tool_calls with
;; an explicit "Message delivered." result — first-class evidence of having
;; spoken, at the cost of a slightly less chat-shaped transcript.
(defmulti render-item
  (fn [item]
    (if (= :summarized (:salience item))
      :summarized
      (:type item))))

(defmethod render-item :summarized [item]
  {:role "user"
   :content (str "[context summary] " (:content (:data item)))})

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

(defmethod render-item :system-note [item]
  {:role "user"
   :content (str "[system] " (:content (:data item)))})

(defmethod render-item :default [item]
  {:role "user"
   :content (pr-str (:data item))})

;; Redesigned per the resident's own feedback (self-review Q7): the old ladder
;; escalated discomfort ("this isn't acceptable"), which he rationally argued
;; against rather than obeyed. Attentive waiting is now legitimate; the nudge
;; guards against true drift, not against patience.
(defn- nudge-for [idle-count]
  (cond
    (<= 3 idle-count 5)
    {:role "system"
     :content "Several quiet turns. If you are waiting on something external, attentive waiting is a legitimate choice — consider a longer wait interval. If not, consider whether anything is genuinely worth initiating."}
    (<= 6 idle-count 8)
    {:role "system"
     :content "Still quiet. Check: is there an ongoing project, observation, or note worth advancing? If nothing needs you, a long deliberate wait is better than filler activity."}
    (>= idle-count 9)
    {:role "system"
     :content "Extended quiet. Review your goals and memory index — pick something meaningful, or settle into a long wait. Do not manufacture busywork."}
    :else nil))

(defn assemble [ctx system-prompt idle-count]
  (let [items (->> (context/fetch-context ctx)
                   (mapv render-item))
        ;; The done-list rides outside the item stream (same family as the
        ;; boot-time memory orientation): a fixed section rebuilt every turn,
        ;; never subject to compaction.
        ledger (context/render-ledger ctx)
        nudge (nudge-for idle-count)]
    (cond-> [{:role "system" :content system-prompt}]
      (seq items) (into items)
      ledger (conj {:role "system" :content ledger})
      nudge (conj nudge))))
