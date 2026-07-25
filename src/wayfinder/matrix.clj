(ns wayfinder.matrix
  (:require [org.httpkit.client :as http]
            [cheshire.core :as json]
            [wayfinder.context :as context]
            [wayfinder.scribe :as scribe]))

(defn send-message [cfg content]
  (let [{:keys [homeserver access-token room-id]} (:matrix cfg)
        ;; millis alone can collide on back-to-back sends; Matrix silently
        ;; dedups identical txn-ids, dropping the second message
        txn-id (str "wf_" (System/currentTimeMillis) "_" (rand-int 1000000))
        url (str homeserver "/_matrix/client/v3/rooms/" room-id "/send/m.room.message/" txn-id)
        body (json/generate-string {:msgtype "m.text" :body content})
        resp @(http/put url
                {:headers {"Content-Type" "application/json"
                           "Authorization" (str "Bearer " access-token)}
                 :body body})]
    (if (and (:status resp) (<= 200 (:status resp) 299))
      {:ok? true :status (:status resp)}
      (do
        (println (format "[matrix] send-message FAILED: status %s%s"
                   (:status resp)
                   (if-let [e (:error resp)] (str " error " (.getMessage e)) "")))
        {:ok? false :status (:status resp)}))))

(defn- extract-room-events [sync-response room-id]
  (get-in sync-response [:rooms :join (keyword room-id) :timeline :events]))

(defn- message-event? [event own-user-id]
  (and (= "m.room.message" (:type event))
       (not= own-user-id (:sender event))))

(defn- parse-sync [body]
  (try (json/parse-string body true) (catch Exception _ nil)))

(defn sync-loop [ctx cfg monitor]
  (let [{:keys [homeserver access-token room-id user-id]} (:matrix cfg)]
    (future
      (loop [since-token nil]
        (let [next-token
              (try
                (let [url (str homeserver "/_matrix/client/v3/sync")
                      params (cond-> {:headers {"Authorization" (str "Bearer " access-token)}
                                      :query-params {:timeout 30000}}
                               since-token (assoc-in [:query-params :since] since-token))
                      resp @(http/get url params)]
                  (if (= 200 (:status resp))
                    (let [body (parse-sync (:body resp))
                          events (extract-room-events body room-id)
                          messages (when since-token
                                    (filter #(message-event? % user-id) events))
                          _ (doseq [msg messages]
                              (swap! ctx context/add-item :user-message
                                {:content (:body (:content msg))})
                              ;; Inline on purpose: the cue lands immediately
                              ;; after the message that triggered it, before
                              ;; the agent is woken, so it can never slip
                              ;; between an action and its result.
                              (scribe/cue-memories ctx cfg (:body (:content msg)))
                              (locking monitor (.notify monitor)))]
                      (:next_batch body))
                    (do (Thread/sleep 5000) since-token)))
                (catch Exception _
                  (Thread/sleep 5000)
                  since-token))]
          (recur next-token))))))
