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

;; --- Typing-aware wake scheduling ---
;; Humans text in bursts: several short messages, each follow-up typed
;; before the previous one was answered. Waking the agent per message
;; made it drop a wall of text between the user's unfinished thoughts.
;; The debounce instead holds the wake until typing stops and a quiet
;; gap passes; the max-hold caps that hold so a user who types forever
;; cannot block delivery forever.

(def ^:private default-typing-debounce-ms 8000)
(def ^:private default-typing-max-hold-ms 45000)

(defn- typing-users [body room-id own-user-id]
  (disj (->> (get-in body [:rooms :join (keyword room-id) :ephemeral :events])
             (filter #(= "m.typing" (:type %)))
             (mapcat #(get-in % [:content :user_ids]))
             set)
        own-user-id))

(defn sync-loop [ctx cfg monitor]
  (let [{:keys [homeserver access-token room-id user-id]} (:matrix cfg)
        debounce-ms (or (:typing-debounce-ms cfg) default-typing-debounce-ms)
        max-hold-ms (or (:typing-max-hold-ms cfg) default-typing-max-hold-ms)
        typing? (atom false)
        wake-at (atom nil)
        burst-start (atom nil)]
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
                          ;; Typing refreshes only on incremental batches:
                          ;; the initial sync's ephemeral block may hold
                          ;; stale pre-restart state, and nothing is pending.
                          _ (when since-token
                              (reset! typing?
                                (not (empty? (typing-users body room-id user-id)))))
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
                              ;; No instant wake: each message re-arms the
                              ;; debounce, sliding the wake forward while
                              ;; the burst keeps going.
                              (reset! burst-start (or @burst-start (System/currentTimeMillis)))
                              (reset! wake-at (+ (System/currentTimeMillis) debounce-ms)))]
                      (:next_batch body))
                    (do (Thread/sleep 5000) since-token)))
                (catch Exception _
                  (Thread/sleep 5000)
                  since-token))]
          (recur next-token))))
    ;; The sync long-poll can sleep up to 30s, so the wake needs its own
    ;; thread: a settled burst would otherwise wait out the poll.
    (future
      (loop []
        (Thread/sleep 1000)
        (try
          (when-let [wake @wake-at]
            (let [now (System/currentTimeMillis)
                  held-too-long? (when-let [started @burst-start]
                                   (> (- now started) max-hold-ms))]
              (when (or (and (not @typing?) (>= now wake)) held-too-long?)
                (reset! wake-at nil)
                (reset! burst-start nil)
                (println "[matrix] burst settled — waking agent")
                (locking monitor (.notify monitor)))))
          (catch Throwable t
            (println (str "[matrix] wake ticker error: " (.getMessage t)))))
        (recur)))))
