(ns wayfinder.matrix
  (:require [org.httpkit.client :as http]
            [cheshire.core :as json]
            [wayfinder.context :as context]
            [wayfinder.dispatch :as dispatch]))

(defn send-message [cfg content]
  (let [{:keys [homeserver access-token room-id]} (:matrix cfg)
        txn-id (str "wf_" (System/currentTimeMillis))
        url (str homeserver "/_matrix/client/v3/rooms/" room-id "/send/m.room.message/" txn-id)
        body (json/generate-string {:msgtype "m.text" :body content})]
    @(http/put url
       {:headers {"Content-Type" "application/json"
                  "Authorization" (str "Bearer " access-token)}
        :body body})))

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
                              (let [notif-id (:next-id @ctx)]
                                (swap! dispatch/pending-messages assoc notif-id (:body (:content msg)))
                                (swap! ctx context/add-item :notification
                                  {:content (str "New message received (id: " notif-id ")")})
                                (locking monitor (.notify monitor))))]
                      (:next_batch body))
                    (do (Thread/sleep 5000) since-token)))
                (catch Exception _
                  (Thread/sleep 5000)
                  since-token))]
          (recur next-token))))))
