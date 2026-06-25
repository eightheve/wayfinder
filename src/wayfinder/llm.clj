(ns wayfinder.llm
  (:require [org.httpkit.client :as http]
            [cheshire.core :as json]))

(defn- trunc [s max-len]
  (let [s (str s)]
    (if (> (count s) max-len)
      (str (subs s 0 max-len) "...")
      s)))

(defn complete [base-url api-key model messages tools reasoning-effort]
  (let [url (str base-url "/chat/completions")
        body (json/generate-string
               (cond-> {:model model
                        :reasoning {:effort (or reasoning-effort "medium")}
                        :messages messages}
                 (seq tools) (assoc :tools tools)))
        _ (println (format "[llm] Sending: %d messages, %d chars" (count messages) (count body)))
        start (System/currentTimeMillis)
        resp @(http/post url
               {:headers {"Content-Type" "application/json"
                          "Authorization" (str "Bearer " api-key)}
                :body body
                :timeout 120000})
        elapsed (- (System/currentTimeMillis) start)]
    (if (:error resp)
      (do
        (println (format "[llm] Request error after %d ms: %s" elapsed (.getMessage (:error resp))))
        (throw (ex-info (str "LLM request error: " (.getMessage (:error resp)))
                 {:error (:error resp)})))
      (do
        (println (format "[llm] Response: status %d, %d ms" (:status resp) elapsed))
        (if (= 200 (:status resp))
          (-> (:body resp)
              (json/parse-string true)
              :choices
              first
              :message)
          (do
            (println (format "[llm] Request failed: body: %s" (trunc (:body resp) 500)))
            (throw (ex-info (str "LLM request failed: status " (:status resp))
                     {:status (:status resp)
                      :body (:body resp)}))))))))
