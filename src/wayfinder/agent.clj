(ns wayfinder.agent
  (:require [wayfinder.context :as context]
            [wayfinder.prompt :as prompt]
            [wayfinder.llm :as llm]
            [wayfinder.tools :as tools]
            [wayfinder.custom-tools :as custom-tools]
            [wayfinder.dispatch :as dispatch]
            [wayfinder.compactor :as compactor]
            [wayfinder.scribe :as scribe]
            [wayfinder.jev :as jev]
            [wayfinder.matrix :as matrix]
            [cheshire.core :as json]
            [clojure.edn]
            [clojure.pprint]
            [clojure.set]
            [clojure.string])
  (:import [java.io File]))

(def default-delay 5000)

(defn load-system-prompt [dir]
  (let [files (->> (file-seq (File. dir))
                   (filter #(.isFile %))
                   (filter #(.endsWith (.getName %) ".md"))
                   (sort-by #(.getName %)))]
    (->> (map slurp files)
         (clojure.string/join "\n\n"))))

(defn parse-tool-calls [response]
  (when-let [calls (:tool_calls response)]
    (for [call calls]
      (let [func (:function call)]
        {:action-type (keyword (:name func))
         :params (try (json/parse-string (:arguments func) true)
                      (catch Exception _ {}))
         :call-id (:id call)}))))

(defn- context-file [cfg]
  (str (or (:state-dir cfg) "/var/lib/wayfinder") "/context.edn"))

(defn save-context
  "Persist live context after every turn (atomic write) so a restart
   resurrects the session instead of rebooting an amnesiac."
  [ctx cfg]
  (try
    (let [f (java.io.File. (context-file cfg))
          tmp (java.io.File. (str (context-file cfg) ".tmp"))]
      (.mkdirs (.getParentFile f))
      (spit tmp (pr-str @ctx))
      (.renameTo tmp f))
    (catch Exception e
      (println (format "[agent] save-context failed: %s" (.getMessage e))))))

(defn load-context [cfg]
  (try
    (let [f (java.io.File. (context-file cfg))]
      (when (.exists f)
        (let [state (clojure.edn/read-string (slurp f))]
          (if (and (map? state) (vector? (:items state)) (int? (:next-id state)))
            ;; A context.edn written before the ledger existed resurrects with
            ;; an empty done-list rather than nil — a reset costs the history,
            ;; never the mechanism.
            (let [state (update state :ledger #(if (vector? %) % []))]
              (println (format "[agent] Resurrected context: %d items, next-id %d, %d ledger entries"
                         (count (:items state)) (:next-id state) (count (:ledger state))))
              state)
            (do (println "[agent] context.edn malformed — starting fresh")
                nil)))))
    (catch Exception e
      (println (format "[agent] load-context failed (%s) — starting fresh" (.getMessage e)))
      nil)))

(defn dump-context [ctx cfg]
  (try
    (let [dir (str (or (:state-dir cfg) "/var/lib/wayfinder") "/debug")]
      (.mkdirs (java.io.File. dir))
      (spit (str dir "/context") (with-out-str (clojure.pprint/pprint @ctx))))
    (catch Exception e
      (println (format "[agent] dump-context failed: %s" (.getMessage e))))))

;; Forward reference: the drift nudge lives with the other Jev machinery
;; below (it shares recent-context-lines), but call-llm above needs it first.
(declare jev-nudge)

(defn call-llm [ctx cfg system-prompt idle-count]
  ;; Jev's drift score, when it has an opinion, replaces the bare idle-count
  ;; nudge — judgment over arithmetic; the count ladder is the fallback when
  ;; Jev has no opinion.
  (let [messages (prompt/assemble @ctx system-prompt idle-count (jev-nudge ctx cfg idle-count))
        agent-cfg (get-in cfg [:agents :main])
        base-url (:base-url agent-cfg)
        api-key (:api-key agent-cfg)
        model (:model agent-cfg)
        effort (:reasoning-effort agent-cfg)]
    (println (format "[agent] Calling LLM (%d items in context)" (count (:items @ctx))))
    (llm/complete base-url api-key model messages (custom-tools/definitions) effort)))

(def ^:private max-result-length 10000)

(defn- token-set [s]
  (set (remove empty? (clojure.string/split (clojure.string/lower-case (str s)) #"[^\p{L}\p{N}]+"))))

(defn- similarity
  "Word-set Jaccard similarity, 0.0-1.0."
  [a b]
  (let [ta (token-set a) tb (token-set b)]
    (if (or (empty? ta) (empty? tb))
      0.0
      (/ (double (count (clojure.set/intersection ta tb)))
         (count (clojure.set/union ta tb))))))

(def ^:private resend-similarity-threshold 0.6)

(defn- trunc-result [c]
  (let [c (str c)]
    (if (> (count c) max-result-length)
      (str (subs c 0 max-result-length) "...")
      c)))

;; The Jev state builders (send gate, drift nudge) render whole context lines
;; into one request, and those states are token-budget bound like every other
;; Jev call: a single 10k-char tool result in the middle of the context block
;; can blow the ~32k-token request. Rendered lines get their own much smaller
;; cap than stored results.
(def ^:private context-line-max 1500)

(defn- trunc [s max-len]
  (let [s (str s)]
    (if (> (count s) max-len)
      (str (subs s 0 max-len) "...")
      s)))

(defn- ledger-opts [cfg]
  {:cap (or (:ledger-cap cfg) 30)
   :arg-length (or (:ledger-arg-length cfg) 72)})

(defn- result-ok?
  "Coarse success signal for the ledger: every failure path in this file
   reports itself by prefixing the result content."
  [content]
  (not (re-find #"(?i)^(error|access denied|move failed|delivery failed|send rejected|file not found)"
         (str content))))

;; --- Send gate ---
;; The loop asks for an action every tick; "Message delivered." is not news,
;; so a message answered only by its own receipt used to invite another
;; message, and the agent ended up replying to itself. The gate holds a send
;; when nothing has happened since the last one — softly, as an action-result,
;; never as an error.

(def ^:private delivery-receipt "Message delivered.")

(defn- send-gate-enabled? [cfg]
  (not (false? (:send-gate-enabled? cfg))))

(defn- newsless-result?
  "Bookkeeping chatter carries no information: a delivery receipt or a
   suppressed duplicate is the loop talking to itself."
  [content]
  (let [c (clojure.string/trim (str content))]
    (or (empty? c)
        (= c delivery-receipt)
        (= c "(duplicate result suppressed)"))))

(defn- new-input-since?
  "Did anything informative land after item `id`? A user message or a recall
   result always counts; a tool result counts unless it is pure bookkeeping.
   Memory cues deliberately don't: being reminded of something you already
   know is not new input from the world."
  [items id]
  (boolean
    (some (fn [item]
            (case (:type item)
              :user-message true
              :memory true
              :action-result (not (newsless-result? (get-in item [:data :content])))
              false))
      (filter #(> (:id %) id) items))))

(defn- send-held?
  "True when the last thing in context is this agent's own delivered message
   and nothing has arrived since. Fails open: an unknown, failed or rejected
   previous send holds nothing."
  [ctx-val action-id]
  (let [prior (filterv #(< (:id %) action-id) (:items ctx-val))
        last-send (->> prior
                       (filter #(and (= :action (:type %))
                                     (= :send-message (get-in % [:data :action-type]))))
                       last)
        receipt (when last-send
                  (->> prior
                       (filter #(and (= :action-result (:type %))
                                     (= (:id last-send) (get-in % [:data :caused-by]))))
                       last))]
    (boolean
      (and receipt
           (= delivery-receipt (clojure.string/trim (str (get-in receipt [:data :content]))))
           (not (new-input-since? prior (:id last-send)))))))

(defn- recent-result-contents [ctx n]
  (->> (:items @ctx)
       (filter #(= :action-result (:type %)))
       (take-last n)
       (map #(get-in % [:data :content]))))

;; --- Jev send gate ---
;; Byte identity is not semantic identity: "yes" after a statement the agent
;; already made is filler, but "yes" answering a question the user just asked
;; is a genuine reply — the words alone cannot tell those apart, only the
;; surrounding dialogue can. So the gate renders the recent conversation and
;; asks one question with one threshold: does the candidate message add new
;; meaning at the conversation's current position? Two gates with two
;; thresholds would just double-tune a single behavior. The Jaccard near-dup
;; rejection and the no-new-input hold remain as fallbacks for when Jev has
;; no opinion.

(defn- recent-context-lines
  "One line per informative recent context item, oldest first — the state
   Jev's drift judgment is made against."
  [ctx-val n]
  (->> (:items ctx-val)
       (keep (fn [item]
               (case (:type item)
                 :user-message (str "[user] " (get-in item [:data :content]))
                 :action-result (let [c (get-in item [:data :content])]
                                  (when-not (newsless-result? c)
                                    (str "[tool result|" (get-in item [:data :caused-by]) "] " (trunc c context-line-max))))
                 :memory (str "[memory recall] " (trunc (get-in item [:data :content]) context-line-max))
                 :memory-cue (str "[memory cue] " (get-in item [:data :content]))
                 :system-note (str "[system] " (get-in item [:data :content]))
                 nil)))
       (take-last n)
       (clojure.string/join "\n")))

(defn- conversation-lines
  "One line per dialogue beat, oldest first — only the user's messages and
   the agent's own sends. This is the raw exchange, not the annotated ledger:
   the send gate must see the actual back-and-forth to tell a fresh answer
   to a new question apart from a repeat of what was already said."
  [ctx-val n]
  (->> (:items ctx-val)
       (keep (fn [item]
               (case (:type item)
                 :user-message (str "user: " (get-in item [:data :content]))
                 :action (when (= :send-message (get-in item [:data :action-type]))
                           (str "agent: " (get-in item [:data :params :content])))
                 nil)))
       (take-last n)
       (map #(trunc % 400))
       (clojure.string/join "\n")))

(defn- jev-send-verdict
  "One Jev call, one question: does the candidate batch add genuinely new
   semantic content given the dialogue so far? Returns a map
   {:verdict :send-or-:hold :p probability :floor threshold}, or nil when
   Jev gives no opinion (caller uses heuristics)."
  [ctx cfg content]
  (when (and (jev/available? cfg) (send-gate-enabled? cfg))
    (let [threshold (or (:jev-send-threshold cfg) 0.5)
          state (str "RECENT CONVERSATION (chronological):\n"
                     (conversation-lines @ctx 10)
                     "\n\nCANDIDATE MESSAGE(S) the agent wants to send next:\n"
                     content)
          result (jev/evaluate cfg state
                   {"adds-meaning"
                    {:type "noul"
                     :instructions (str "The conversation so far is in the state. Does the candidate message add genuinely new semantic content for the user — "
                                        "something they do not already know or have from the conversation's current position? A short answer ('yes', 'ok', 'done') is NEW content when it answers a question or confirms something pending in the conversation; the SAME words with nothing new pending are filler. Restating, rephrasing, or continuing what the agent already said redundantly is NOT new.")
                     :criteria {"true" "User gains information, answers, or closure they did not already have"
                                "false" "Nothing new: restatement, bare filler, or self-reply chatter"}}})
          p (when result (jev/noul-of (:answers result) "adds-meaning"))
          verdict (cond
                    (nil? p) nil
                    (>= p threshold) :send
                    :else :hold)]
      (println (format "[agent] Jev send gate: semantic-novelty %s (send floor %.2f) → %s"
                 (if p (format "p=%.3f" (double p)) "p=n/a")
                 (double threshold)
                 (if verdict (name verdict) "no-opinion")))
      (when verdict
        {:verdict verdict :p p :floor threshold}))))

;; --- Jev drift nudge ---
;; The idle-count ladder is arithmetic: it escalates by count alone, whether
;; the quiet is drift or a deliberately chosen wait. Jev reads the actual
;; ledger and recent context and scores drift instead; the count ladder stays
;; as the fallback for whenever Jev has no opinion.
(defn- jev-nudge
  "One Jev score question over the recent ledger + context: how drifted is
   this agent? Returns {:text nudge-text-or-nil} to override the count
   ladder, or nil when there is no opinion (Jev unavailable, failed, or no
   score came back). A nil :text is itself an opinion — the quiet was judged
   chosen, so no nudge fires even at high idle counts."
  [ctx cfg idle-count]
  (when (and (jev/available? cfg) (>= idle-count 3))
    (try
      (let [state (str "The agent has been quiet for " idle-count " turns.\n\n"
                       "RECENT ACTIVITY LEDGER:\n"
                       (or (context/render-ledger @ctx) "(nothing done yet)")
                       "\n\nRECENT CONTEXT:\n"
                       (recent-context-lines @ctx 10))
            result (jev/evaluate cfg state
                     {"drift"
                      {:type "score"
                       :instructions "How stuck is this agent, judging by its recent ledger and context? Purposely waiting for an external event or patiently working a long task is NOT stuck — that is chosen behavior. What matters is drift: filler actions, circular reasoning, or long purposeless quiet."
                       :criteria ["Not stuck: productive, or deliberately and meaningfully waiting — needs nothing"
                                  "Mildly drifting: quiet without clear purpose — a gentle check-in would help"
                                  "Stuck: extended pointless cycling or purposeless silence — a strong nudge is warranted"]}})
            score (:score (get (:answers result) "drift"))]
        (cond
          (nil? score) nil
          (< score 0.75) {:text nil}
          (< score 1.5) {:text (get prompt/nudge-texts 1)}
          (< score 1.9) {:text (get prompt/nudge-texts 2)}
          :else {:text (get prompt/nudge-texts 3)}))
      (catch Exception e
        (println (format "[agent] Jev drift nudge failed (%s) — count-ladder fallback" (.getMessage e)))
        nil))))

;; --- Send batch ---
;; The gate judges per turn, not per bubble: a model that texts like a human
;; may send several short bubbles in one turn, and judging each on its own
;; would hold bubble #2+ as "nothing new since the last send". Multi-bubble
;; texting is legitimate, but the anti-restatement protections must still
;; fire — so all of a turn's sends are weighed as one combined message
;; against ONE gate decision, then delivered bubble by bubble.
(defn- execute-send-batch
  [ctx cfg sends recently-sent]
  (let [contents (mapv #(get-in % [:params :content]) sends)
        combined (clojure.string/join "\n\n" contents)
        ;; Fallback heuristics read the context BEFORE any batch item lands in
        ;; it: next-id acts as the hypothetical first action id, so everything
        ;; already in context is the untouched prior world the gate judges.
        held? (and (send-gate-enabled? cfg)
                   (send-held? @ctx (:next-id @ctx)))
        ;; Jev judges whether the batch adds new meaning when available; a
        ;; nil verdict hands off to the old heuristics.
        jev-result (jev-send-verdict ctx cfg combined)
        jev-verdict (:verdict jev-result)
        near-dup (when-not jev-verdict
                   (some #(when (> (similarity combined %) resend-similarity-threshold) %)
                     @recently-sent))
        ;; Bookkeep every bubble as a first-class action before the verdict:
        ;; each renders as its own tool_call, and whatever the gate decides,
        ;; each gets its own matching result. Ids are the contiguous range
        ;; (dec next-id) yields per add.
        bubbles (loop [remaining sends acc []]
                  (if-let [send (first remaining)]
                    (do (swap! ctx context/add-item :action
                          {:action-type :send-message :params (:params send) :call-id (:call-id send)})
                        (recur (rest remaining)
                               (conj acc [(dec (:next-id @ctx)) send])))
                    acc))]
    (cond
      near-dup
      ;; Fallback backstop against restatement sprees (only when Jev has no
      ;; opinion): recently-sent finally earns its keep. The rejection is
      ;; reported as the tool result so the model learns why nothing was
      ;; delivered. One verdict covers the whole batch, so every bubble is
      ;; rejected.
      (doseq [[action-id send] bubbles]
        (println (format "[agent] send-message REJECTED as near-duplicate (item %d, similarity > %.1f)"
                   action-id resend-similarity-threshold))
        (swap! ctx context/add-item :action-result
          {:caused-by action-id
           :content "Send REJECTED: nearly identical to a message you already sent. The user already has that message — say something genuinely new, or stay silent. Do not respond to this rejection message, it is a purely internal result."})
        ;; Ledger records attempts too: a rejected send did happen, and
        ;; seeing it listed as FAILED is how the pattern becomes visible.
        (swap! ctx context/record-action :send-message (:params send) false (ledger-opts cfg)))

      (or (= :hold jev-verdict) (and (not jev-verdict) held?))
      ;; Softer than the duplicate rejection: the message may be perfectly
      ;; good, it just has nothing to answer. Held, not refused. The Jev hold
      ;; carries its evidence (probability and floor) in the result so the
      ;; agent can tell a system verdict from a failed send.
      (doseq [[action-id send] bubbles]
        (println (format "[agent] send-message HELD (item %d): %s"
                   action-id
                   (if jev-verdict "Jev: nothing new to say" "nothing new since the last delivered message")))
        (swap! ctx context/add-item :action-result
          {:caused-by action-id
           :content (if jev-verdict
                      (format "Message held by the send gate (evaluated as adding no new semantic content to the conversation, p=%s, floor %.2f). It was NOT delivered. Develop genuinely new content or stay quiet — do not respond to this result, it is a purely internal system verdict."
                              (if-let [p (:p jev-result)] (format "%.3f" (double p)) "n/a")
                              (double (or (:floor jev-result) (:jev-send-threshold cfg) 0.5)))
                      "No new input since your last message — hold, or record a note instead. (Message not sent.) Do not respond to this result, it is a purely internal one.")})
        (swap! ctx context/record-action :send-message (:params send) false (ledger-opts cfg)))

      :else
      ;; Deliver bubble by bubble, in order, with a beat between them: a
      ;; human reads bubbles as they arrive — an instant triple-send looks
      ;; robotic.
      (doseq [[i [action-id send]] (map-indexed vector bubbles)]
        (let [content (get-in send [:params :content])
              _ (println (format "[agent] EXEC send-message (item %d)" action-id))
              {:keys [ok? status]} (matrix/send-message cfg content)]
          ;; Send-message is bookkept like every other tool: the action
          ;; renders as a tool_call and gets an explicit result, so the
          ;; model has first-class evidence that it spoke (or failed to).
          (swap! ctx context/add-item :action-result
            {:caused-by action-id
             :content (if ok?
                        "Message delivered."
                        (format "Delivery FAILED (status %s) — the user did NOT receive this message." status))})
          (swap! ctx context/record-action :send-message (:params send) ok? (ledger-opts cfg))
          ;; Only delivered content feeds the near-dup gate: a failed send
          ;; was never seen by the user, so retrying it must not read as a
          ;; duplicate.
          (when ok?
            (swap! recently-sent conj content)
            (swap! recently-sent #(vec (take-last 10 %))))
          (when (< i (dec (count bubbles)))
            (Thread/sleep 400)))))))

(defn execute-and-record [ctx cfg action recently-sent]
  (let [{:keys [action-type params call-id]} action]
    (cond
      (= action-type :reason)
      ;; Native reasoning is per-turn ephemeral: when the model calls reason
      ;; without a thought, what lands in context is an empty husk (dump id
      ;; 253: {:type :reasoning :data {:content nil}}) that renders as an
      ;; assistant message with no content. Record thoughts, drop blanks.
      (let [thought (:thought params)]
        (if (clojure.string/blank? (str thought))
          (println "[agent] REASON with empty content — not recorded")
          (swap! ctx context/add-item :reasoning {:content thought}))
        nil)

      (= action-type :wait)
      (let [secs (max 5 (min 300 (:seconds params)))]
        (println (format "[agent] WAIT %ds" secs))
        ;; Waiting is a decision, not idleness — at any duration. Charging the
        ;; idle counter for it made the cheapest legitimate turn the most
        ;; expensive one, so the model paid the toll with filler instead.
        {:delay (* secs 1000) :deliberate-wait? true})

      (= action-type :send-message)
      ;; Unreachable: process-turn routes every send through
      ;; execute-send-batch. If this fires anyway, routing is broken —
      ;; surface it loudly rather than silently dropping a user's message.
      (do
        (println "[agent] BUG: send-message reached execute-and-record — internal routing bug")
        (swap! ctx context/add-item :action
          {:action-type :send-message :params params :call-id call-id})
        (swap! ctx context/add-item :action-result
          {:caused-by (dec (:next-id @ctx))
           :content "Error: send-message must go through the batch path — internal routing bug"})
        nil)

      :else
      (let [_ (swap! ctx context/add-item :action
                  {:action-type action-type :params params :call-id call-id})
            action-id (dec (:next-id @ctx))
            _ (println (format "[agent] EXEC %s (item %d)" (name action-type) action-id))
            result (cond
                     (= action-type :recall)
                     (do (scribe/recall ctx cfg (:query params))
                         {:content "Memory recall initiated"})

                     (= action-type :append-memory)
                     (do (println (format "[agent] APPEND-MEMORY %s" (:filename params)))
                         {:content (try
                                     (str "Appended to "
                                       (scribe/append-note cfg (:filename params) (:content params)))
                                     (catch Exception e
                                       (str "Error appending: " (.getMessage e))))})

                     (= action-type :move-memory)
                     (do (println (format "[agent] MOVE-MEMORY %s -> %s" (:from params) (:to params)))
                         {:content (try
                                     (let [{:keys [ok?]} (scribe/move-note cfg (:from params) (:to params))]
                                       (if ok?
                                         (format "Moved %s to %s" (:from params) (:to params))
                                         (format "Move failed: %s not found" (:from params))))
                                     (catch Exception e
                                       (str "Error moving: " (.getMessage e))))})

                     (= action-type :delete-memory)
                     (do (println (format "[agent] DELETE-MEMORY %s" (:path params)))
                         {:content (try
                                     (if (:ok? (scribe/delete-note cfg (:path params)))
                                       (format "Deleted %s (moved to trash)" (:path params))
                                       (format "Delete failed: %s not found" (:path params)))
                                     (catch Exception e
                                       (str "Error deleting: " (.getMessage e))))})

                     (= action-type :consolidate-memories)
                     (do (println (format "[agent] CONSOLIDATE -> %s" (:filename params)))
                         {:content (try
                                     (if (seq (:paths params))
                                       (let [{:keys [target deleted]}
                                             (scribe/consolidate-note
                                               cfg (:paths params) (:filename params) (:content params))]
                                         (format "Wrote %s, deleted %d source(s)" target deleted))
                                       "Error: no source paths given")
                                     (catch Exception e
                                       (str "Error consolidating: " (.getMessage e))))})

                     (= action-type :remember)
                     (do (println (format "[agent] REMEMBER %s" (:filename params)))
                         {:content (try
                                     (str "Memory written: "
                                       (scribe/remember-note cfg (:filename params) (:content params)))
                                     (catch Exception e
                                       (str "Error writing memory: " (.getMessage e))))})

                     (= action-type :list-memories)
                     (do (println "[agent] LIST-MEMORIES")
                         {:content (try (scribe/list-memories cfg)
                                        (catch Exception e
                                          (str "Error listing memories: " (.getMessage e))))})

                     (= action-type :pin-item)
                     (let [id (:id params)]
                       (swap! ctx context/update-item id {:pinned true})
                       (println (format "[agent] PIN item %d" id))
                       {:content (format "Pinned item %d" id)})

                     (= action-type :unpin-item)
                     (let [id (:id params)]
                       (swap! ctx context/update-item id {:pinned false})
                       (println (format "[agent] UNPIN item %d" id))
                       {:content (format "Unpinned item %d" id)})

                     (= action-type :curate-memories)
                     (do (future (try (scribe/curate cfg)
                                  (catch Exception e
                                    (println (format "[agent] Curation failed: %s" (.getMessage e))))))
                         {:content "Memory curation initiated"})

                     :else
                     (if (custom-tools/exec-for action-type)
                       (custom-tools/invoke action-type params)
                       (try (dispatch/execute-action {:action-type action-type
                                                      :message-id (:message-id params)
                                                      :command (:command params)
                                                      :path (:path params)})
                            (catch Exception e
                              (println (format "[agent] ERROR in %s: %s" (name action-type) (.getMessage e)))
                              {:content (str "Error: " (.getMessage e))}))))
            content (trunc-result (:content result))
            duplicate? (some #(= content %) (recent-result-contents ctx 3))
            ;; Always record a result: prompt.clj renders every :action as an
            ;; assistant tool_call, and a tool_call without a matching role:"tool"
            ;; message is rejected by OpenAI-compatible endpoints.
            _ (swap! ctx context/add-item :action-result
                {:caused-by action-id
                 :content (if duplicate? "(duplicate result suppressed)" content)})
            ;; Every tool that produced a result lands in the done-list.
            ;; reason and wait deliberately don't: the ledger answers "what
            ;; have I already done", and thinking or waiting is not doing.
            _ (swap! ctx context/record-action action-type params (result-ok? content) (ledger-opts cfg))]
        nil))))

(defn process-turn [ctx cfg system-prompt idle-count recently-sent]
  (try
    (let [response (call-llm ctx cfg system-prompt idle-count)
          actions (seq (parse-tool-calls response))]
      ;; Preserve the provider's native reasoning before any action executes:
      ;; the assistant message carries its thinking trace as :reasoning, which
      ;; was previously dropped entirely. Recording it here gives the item an
      ;; id (and rendered position) before the turn's tool_calls, keeps the
      ;; model's own reasoning visible across turns (coherence), and turns
      ;; otherwise-lost bytes into stable prefix the next call can hit. The
      ;; blank-guard mirrors the REASON-with-empty-content handling.
      (when (and (get cfg :preserve-reasoning true)
                 (not (clojure.string/blank? (str (:reasoning response)))))
        (swap! ctx context/add-item :reasoning {:content (:reasoning response)}))
      (if-let [actions actions]
        ;; All of a turn's sends form ONE batch judged by one gate decision
        ;; (multi-bubble texting is legitimate; see execute-send-batch);
        ;; everything else executes in its original order, exactly as before.
        (let [{sends true others false}
              (group-by #(= :send-message (:action-type %)) actions)]
          (loop [actions others wait-info nil productive? false]
            (if-let [action (first actions)]
              (let [result (execute-and-record ctx cfg action recently-sent)
                    productive? (or productive?
                                  (and (not= :wait (:action-type action))
                                       (not= :reason (:action-type action))))]
                (recur (rest actions) (or wait-info result) productive?))
              (do
                ;; Sends go last, as one batch: the gate weighs their
                ;; combined content against the context the other actions
                ;; just produced.
                (when (seq sends)
                  (execute-send-batch ctx cfg sends recently-sent))
                {:delay (:delay wait-info)
                 :productive? (or productive? (boolean (seq sends)))
                 :waiting? (boolean (:deliberate-wait? wait-info))}))))
        {:delay nil :productive? false}))
    (catch Exception e
      (println (format "[agent] Turn error: %s" (.getMessage e)))
      {:delay default-delay :productive? false})))

(defn start-message-watcher [ctx cfg monitor]
  (matrix/sync-loop ctx cfg monitor))

(defn run [cfg]
  (let [_ (System/setProperty "user.dir" (or (:home-dir cfg) "/home/wayfinder"))
        ctx (atom (or (load-context cfg) {:items [] :next-id 0 :ledger []}))
        ;; On a genuinely fresh boot, orient the resident with its own
        ;; long-term memory index so rebirth starts from what it knows,
        ;; not from zero.
        _ (when (empty? (:items @ctx))
            (try
              (let [index (scribe/list-memories cfg)]
                (when (and index (not= index "No memories stored"))
                  (swap! ctx context/add-item :memory
                    {:content (str "Long-term memory index (from before this boot):\n" index)})
                  (println "[agent] Fresh boot: injected long-term memory index")))
              (catch Exception e
                (println (format "[agent] memory orientation failed: %s" (.getMessage e))))))
        system-prompt (load-system-prompt (or (:prompts-dir cfg) "prompts"))
        monitor (Object.)
        ;; Compaction rewrites early items, so every compaction busts the
        ;; entire prefix cache. Compacting half as often with the same target
        ;; amortizes that bust over a longer stable window: each cache flush
        ;; buys ~2x the tokens of shared prefix before the next one.
        threshold (or (:compact-threshold cfg) 16000)
        target (or (:compact-target cfg) 5000)
        cooldown-ms (* (or (:compact-cooldown cfg) 300) 1000)
        last-compact (atom (System/currentTimeMillis))
        curate-interval (* (or (:curate-interval cfg) 1800) 1000)
        last-curate (atom (System/currentTimeMillis))
        idle-count (atom 0)
        recently-sent (atom [])
        _ (custom-tools/start! cfg)]
    (start-message-watcher ctx cfg monitor)
    (println (format "Wayfinder agent running. Connected to Matrix. Compact threshold=%d tokens target=%d tokens cooldown=%ds curate-interval=%ds"
               threshold target (or (:compact-cooldown cfg) 300) (or (:curate-interval cfg) 1800)))
    (loop [delay default-delay waited? false marker-id nil]
      (let [start (System/currentTimeMillis)
            pre-sleep-id (dec (:next-id @ctx))]
        (try
          (locking monitor (.wait monitor delay))
          (catch InterruptedException _))
        ;; A deliberate wait leaves a running total, not a per-call log: fold
        ;; this sleep's wall-clock time into the open wait-marker, or open one.
        ;; Real elapsed, not requested seconds — an arriving message cuts the
        ;; sleep short. A first wait cut short by input leaves no marker at
        ;; all: it would render after the message that ended it and read as
        ;; waiting that never happened.
        (let [marker-id
              (if-not waited?
                marker-id
                (let [now (System/currentTimeMillis)
                      elapsed (- now start)
                      live? (context/live-wait-marker? @ctx marker-id)
                      interrupted? (new-input-since? (:items @ctx) pre-sleep-id)]
                  (cond
                    live? (do (swap! ctx context/accrue-wait marker-id elapsed)
                              marker-id)
                    interrupted? nil
                    :else (do (swap! ctx context/add-item :wait-marker
                                {:since-ms (- now elapsed)
                                 :elapsed-ms elapsed
                                 :waits 1})
                              (dec (:next-id @ctx))))))
              token-count (context/token-estimate (context/fetch-context @ctx))
              needs-compact (context/needs-compact? @ctx threshold)
              elapsed-since-compact (- start @last-compact)
              can-compact (> elapsed-since-compact cooldown-ms)
              elapsed-since-curate (- start @last-curate)
              can-curate (> elapsed-since-curate curate-interval)]
          (when (and needs-compact can-compact)
            (println (format "[agent] Context at %d tokens (threshold %d), triggering compaction"
                       token-count threshold))
            (reset! last-compact start)
            (try
              (compactor/compact ctx cfg target)
              (catch Exception e
                (println (format "[agent] Compaction failed: %s" (.getMessage e))))))
          (when can-curate
            (println (format "[agent] %ds since last curation, triggering memory curation"
                       (int (/ elapsed-since-curate 1000))))
            (reset! last-curate start)
            (future
              (try
                (scribe/curate cfg)
                (catch Exception e
                  (println (format "[agent] Curation failed: %s" (.getMessage e)))))))
          (doseq [err (custom-tools/new-errors)]
            (swap! ctx context/add-item :system-note
              {:content (custom-tools/format-error err)}))
          (let [next-result (process-turn ctx cfg system-prompt @idle-count recently-sent)]
            (cond
              (:productive? next-result) (reset! idle-count 0)
              ;; deliberate long wait: hold the counter — patience is a
              ;; chosen state, not accumulating idleness
              (:waiting? next-result) nil
              :else (swap! idle-count inc))
            (save-context ctx cfg)
            (dump-context ctx cfg)
            ;; The marker stays open only across an unbroken run of deliberate
            ;; waits: a productive turn or fresh input ends the run, and the
            ;; next wait opens a new marker (the old one stays as trace).
            (recur (or (:delay next-result) default-delay)
                   (boolean (:waiting? next-result))
                   (when (and marker-id
                              (:waiting? next-result)
                              (not (:productive? next-result))
                              (not (new-input-since? (:items @ctx) marker-id)))
                     marker-id))))))))
