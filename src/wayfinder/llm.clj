(ns wayfinder.llm
  (:require [org.httpkit.client :as http]
            [cheshire.core :as json]))

(defn complete [base-url api-key model messages tools]
  (let [url (str base-url "/chat/completions")
        body (json/generate-string
               (cond-> {:model model
                        :reasoning {:effort "medium"}
                        :messages messages}
                 (seq tools) (assoc :tools tools)))
        resp @(http/post url
               {:headers {"Content-Type" "application/json"
                          "Authorization" (str "Bearer " api-key)}
                :body body})]
    (if (= 200 (:status resp))
      (-> (:body resp)
          (json/parse-string true)
          :choices
          first
          :message)
      (throw (ex-info (str "LLM request failed: " (:body resp))
               {:status (:status resp)
                :body (:body resp)})))))
