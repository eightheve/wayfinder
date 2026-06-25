(ns wayfinder.agent
  (:require [wayfinder.context :as context]
            [wayfinder.prompt :as prompt]
            [wayfinder.llm :as llm]
            [wayfinder.tools :as tools]
            [wayfinder.dispatch :as dispatch]
            [wayfinder.compactor :as compactor]
            [wayfinder.scribe :as scribe]
            [wayfinder.matrix :as matrix]
            [cheshire.core :as json]
            [clojure.pprint])
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

(defn dump-context [ctx cfg]
  (let [dir (str (or (:state-dir cfg) "/var/lib/wayfinder") "/debug")]
    (.mkdirs (java.io.File. dir))
    (spit (str dir "/context") (with-out-str (clojure.pprint/pprint @ctx)))))

(defn call-llm [ctx cfg system-prompt]
  (let [messages (prompt/assemble @ctx system-prompt)
        base-url (:base-url cfg)
        api-key (:api-key cfg)
        model (get-in cfg [:models :small])]
    (println (format "[agent] Calling LLM (%d items in context)" (count (:items @ctx))))
    (llm/complete base-url api-key model messages tools/tool-definitions "high")))

(defn execute-and-record [ctx cfg action]
  (let [{:keys [action-type params call-id]} action]
    (if (= action-type :reason)
      (do (swap! ctx context/add-item :reasoning {:content (:thought params)})
          nil)
      (if (= action-type :wait)
        (let [secs (max 5 (min 300 (:seconds params)))]
          (println (format "[agent] WAIT %ds" secs))
          {:delay (* secs 1000)})
        (let [_ (swap! ctx context/add-item :action
                  {:action-type action-type :params params :call-id call-id})
              action-id (dec (:next-id @ctx))
              _ (println (format "[agent] EXEC %s (item %d)" (name action-type) action-id))
              result (if (= action-type :send-message)
                       (do (matrix/send-message cfg (:content params))
                           {:content "Message sent"})
                       (if (= action-type :recall)
                         (do (scribe/recall ctx cfg (:query params))
                             {:content "Memory recall initiated"})
                         (try (dispatch/execute-action {:action-type action-type
                                                      :message-id (:message-id params)
                                                      :command (:command params)
                                                      :path (:path params)})
                            (catch Exception e
                              (println (format "[agent] ERROR in %s: %s" (name action-type) (.getMessage e)))
                              {:content (str "Error: " (.getMessage e))}))))
              _ (swap! ctx context/add-item :action-result
                  {:caused-by action-id :content (:content result)})]
          nil)))))

(defn process-turn [ctx cfg system-prompt]
  (let [response (call-llm ctx cfg system-prompt)]
    (if-let [actions (seq (parse-tool-calls response))]
      (loop [actions actions wait-info nil]
        (if-let [action (first actions)]
          (let [result (execute-and-record ctx cfg action)]
            (recur (rest actions) (or wait-info result)))
          wait-info))
      nil)))

(defn start-message-watcher [ctx cfg monitor]
  (matrix/sync-loop ctx cfg monitor))

(defn run [cfg]
  (let [ctx (atom {:items [] :next-id 0})
        system-prompt (load-system-prompt (or (:prompts-dir cfg) "prompts"))
        monitor (Object.)
        threshold (or (:compact-threshold cfg) 60)
        target (or (:compact-target cfg) 40)
        cooldown-ms (* (or (:compact-cooldown cfg) 120) 1000)
        last-compact (atom 0)]
    (start-message-watcher ctx cfg monitor)
    (println (format "Wayfinder agent running. Connected to Matrix. Compact threshold=%d target=%d cooldown=%ds"
               threshold target (or (:compact-cooldown cfg) 120)))
    (loop [delay default-delay]
      (let [start (System/currentTimeMillis)]
        (try
          (locking monitor (.wait monitor delay))
          (catch InterruptedException _))
        (let [item-count (count (:items @ctx))
              needs-compact (context/needs-compact? @ctx threshold)
              elapsed-since (- start @last-compact)
              can-compact (> elapsed-since cooldown-ms)]
          (when (and needs-compact can-compact)
            (println (format "[agent] Context at %d items (threshold %d), triggering compaction"
                       item-count threshold))
            (reset! last-compact start)
            (try
              (compactor/compact ctx cfg target)
              (catch Exception e
                (println (format "[agent] Compaction failed: %s" (.getMessage e))))))
          (let [next-result (process-turn ctx cfg system-prompt)]
            (dump-context ctx cfg)
            (recur (or (:delay next-result) default-delay))))))))
