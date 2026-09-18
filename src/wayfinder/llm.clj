(ns wayfinder.llm
  (:require [org.httpkit.client :as http]
            [cheshire.core :as json]))

(defn- trunc [s max-len]
  (let [s (str s)]
    (if (> (count s) max-len)
      (str (subs s 0 max-len) "...")
      s)))

(defn embed [base-url api-key model text]
  (let [url (str base-url "/embeddings")
        body (json/generate-string {:model model :input text})
        _ (println (format "[llm] Embedding: %d chars" (count text)))
        start (System/currentTimeMillis)
        resp @(http/post url
               {:headers {"Content-Type" "application/json"
                          "Authorization" (str "Bearer " api-key)}
                :body body
                :timeout 30000})
        elapsed (- (System/currentTimeMillis) start)]
    (if (:error resp)
      (do
        (println (format "[llm] Embedding error after %d ms: %s" elapsed (.getMessage (:error resp))))
        nil)
      (if (= 200 (:status resp))
        (let [data (-> (:body resp) (json/parse-string true))
              embedding (get-in data [:data 0 :embedding])]
          (println (format "[llm] Embedding OK: %d dims, %d ms" (count embedding) elapsed))
          embedding)
        (do
          (println (format "[llm] Embedding failed: status %d, body: %s" (:status resp) (trunc (:body resp) 500)))
          nil)))))

;; Prefix-cache telemetry. The provider reports how many prompt tokens were
;; served from its cache (usage.prompt_tokens_details.cached_tokens); tracking
;; it per call shows whether context edits are preserving or busting the cache,
;; which is otherwise invisible from the outside.
(defonce cache-stats (atom {:calls 0 :cached 0 :prompt 0 :recent []}))

(defn- note-cache-usage!
  "Update the running cache-hit totals and log one line per call. Calls whose
   usage lacks cached_tokens or prompt_tokens are skipped — there is nothing
   meaningful to log for them."
  [usage]
  (let [prompt (:prompt_tokens usage)
        cached (get-in usage [:prompt_tokens_details :cached_tokens])]
    (when (and (number? prompt) (pos? prompt) (number? cached))
      (let [pct (* 100.0 (/ (double cached) (double prompt)))
            r (swap! cache-stats
                     ;; Pass this call's totals as args: destructuring the old
                     ;; state would shadow them and accumulate the state twice.
                     (fn [{:keys [calls cached prompt recent]} add-cached add-prompt]
                       {:calls (inc calls)
                        :cached (+ cached add-cached)
                        :prompt (+ prompt add-prompt)
                        :recent (take-last 50 (conj recent pct))})
                     cached prompt)]
        (println (format "[llm] cache: %d/%d tokens (%.1f%%) — last %d avg %.1f%%"
                         cached prompt pct
                         (count (:recent r))
                         (/ (reduce + (:recent r)) (count (:recent r)))))))))

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
          (let [data (-> (:body resp) (json/parse-string true))
                message (-> data :choices first :message)]
            (note-cache-usage! (:usage data))
            message)
          (do
            (println (format "[llm] Request failed: body: %s" (trunc (:body resp) 500)))
            (throw (ex-info (str "LLM request failed: status " (:status resp))
                     {:status (:status resp)
                      :body (:body resp)}))))))))
