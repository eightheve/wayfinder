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
        resp @(http/post url
               {:headers {"Content-Type" "application/json"
                          "Authorization" (str "Bearer " api-key)}
                :body body
                :timeout 120000})]
    (if (:error resp)
      (do
        (println (format "[llm] Request error: %s" (.getMessage (:error resp))))
        (throw (ex-info (str "LLM request error: " (.getMessage (:error resp)))
                 {:error (:error resp)})))
      (if (= 200 (:status resp))
        (-> (:body resp)
            (json/parse-string true)
            :choices
            first
            :message)
        (do
          (println (format "[llm] Request failed: status %d, body: %s"
                     (:status resp) (trunc (:body resp) 500)))
          (throw (ex-info (str "LLM request failed: status " (:status resp))
                   {:status (:status resp)
                    :body (:body resp)})))))))
