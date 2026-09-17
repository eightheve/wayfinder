(ns wayfinder.jev
  "Client for the TypeSafe Jev 'System One' decision API.

  Jev is not an LLM: POST /v1/systemone evaluates one shared `state` against
  many typed questions (noul / choice / score) in a single parallel pass and
  returns calibrated probabilities per question. It cannot generate text —
  which is exactly why wayfinder uses it for decision points that previously
  needed an LLM call or a brittle similarity heuristic.

  Every call site treats nil as 'no opinion' and falls back to its old
  heuristic — Jev must never break an agent loop."
  (:require [org.httpkit.client :as http]
            [cheshire.core :as json]))

(def default-base-url "https://api.typesafe.ai")
(def default-model "jev-latest")

(defn api-key [cfg]
  ;; Env wins over config: the key lives in the service's EnvironmentFile
  ;; (.env), never in the checked-out config.
  (or (System/getenv "JEV_API_KEY")
      (get-in cfg [:jev :api-key])))

(defn available? [cfg]
  (boolean (seq (api-key cfg))))

(defn- trunc [s n]
  (let [s (str s)]
    (if (> (count s) n)
      (str (subs s 0 n) "...")
      s)))

;; Docs say one Choice question supports up to 255 options; the total question
;; count per call is undocumented, so keep batches comfortably small and let
;; the caller chunk. A failing batch mid-sequence loses nothing — scores from
;; completed chunks still apply.
(def max-questions-per-call 60)

(def ^:private max-retries 3)

(def ^:private question-id-max 32)

(defn- truncate-id [id]
  (subs (str id) 0 (min question-id-max (count (str id)))))

(defn evaluate
  "Evaluate `state` (string or structured data) against a map of
   question-id -> question (noul/choice/score maps). Returns
   {:answers {id answer} :usage {...}} with String keys, or nil on failure.
   Retries 429/529 with exponential backoff per the API docs."
  [cfg state questions]
  (when (and (available? cfg) (seq questions))
    (let [api-key (api-key cfg)
          base-url (or (get-in cfg [:jev :base-url]) default-base-url)
          model (or (get-in cfg [:jev :model]) default-model)
          questions (into {} (map (fn [[k v]] [(truncate-id k) v])) questions)
          url (str base-url "/v1/systemone")
          body (json/generate-string {:state state :model model :questions questions})
          start (System/currentTimeMillis)]
      (loop [attempt 1]
        (let [resp (try
                     @(http/post url
                        {:headers {"Content-Type" "application/json"
                                   "Authorization" (str "Bearer " api-key)}
                         :body body
                         :timeout 30000})
                     (catch Exception e
                       {:error e}))]
          (cond
            (= 200 (:status resp))
            (let [elapsed (- (System/currentTimeMillis) start)
                  data (json/parse-string (:body resp) true)
                  ;; Cheshire keywordizes nested keys, but every lookup in the
                  ;; codebase (and in callers building "i_<id>" / "m_<n>" ids
                  ;; that may exceed keyword-interning sanity) uses strings:
                  ;; de-keywordize the answers map here, once, at the boundary.
                  answers (into {} (map (fn [[k v]] [(name k) v])) (:answers data))]
              (println (format "[jev] %d questions on %d-chars state — OK in %d ms (%s tokens in)"
                         (count questions) (count body) elapsed
                         (get-in data [:usage :input_tokens])))
              {:answers answers :usage (:usage data) :model (:model data)})

            (and (contains? #{429 529} (:status resp)) (< attempt max-retries))
            (let [backoff-ms (* 250 (bit-shift-left 1 attempt))]
              (println (format "[jev] status %d — retry %d in %d ms" (:status resp) attempt backoff-ms))
              (Thread/sleep backoff-ms)
              (recur (inc attempt)))

            :else
            (do
              (println (format "[jev] FAILED (status %s, attempt %d): %s"
                         (:status resp) attempt
                         (trunc (or (:body resp)
                                    (some-> (:error resp) .getMessage)) 300)))
              nil)))))))

(defn noul-of
  "Probability (0..1) of 'yes' for a noul answer, keyed by (truncated) id."
  [answers id]
  (get-in answers [(truncate-id id) :noul]))

(defn choice-of
  "Highest-probability option for a choice answer."
  [answers id]
  (:choice (get answers (truncate-id id))))

(defn confidence-of
  [answers id]
  (:confidence (get answers (truncate-id id))))

(defn chunk-batch
  "Split a seq into batches of at most max-questions-per-call — the unit in
   which Jev questions are fanned out per call."
  [coll]
  (partition-all max-questions-per-call coll))
