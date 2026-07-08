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
  (try
    (let [dir (str (or (:state-dir cfg) "/var/lib/wayfinder") "/debug")]
      (.mkdirs (java.io.File. dir))
      (spit (str dir "/context") (with-out-str (clojure.pprint/pprint @ctx))))
    (catch Exception e
      (println (format "[agent] dump-context failed: %s" (.getMessage e))))))

(defn call-llm [ctx cfg system-prompt idle-count]
  (let [messages (prompt/assemble @ctx system-prompt idle-count)
        base-url (:base-url cfg)
        api-key (:api-key cfg)
        agent-cfg (get-in cfg [:agents :main])
        model (:model agent-cfg)
        effort (:reasoning-effort agent-cfg)]
    (println (format "[agent] Calling LLM (%d items in context)" (count (:items @ctx))))
    (llm/complete base-url api-key model messages tools/tool-definitions effort)))

(def ^:private max-result-length 10000)

(defn- trunc-result [c]
  (let [c (str c)]
    (if (> (count c) max-result-length)
      (str (subs c 0 max-result-length) "...")
      c)))

(defn- recent-result-contents [ctx n]
  (->> (:items @ctx)
       (filter #(= :action-result (:type %)))
       (take-last n)
       (map #(get-in % [:data :content]))))

(defn execute-and-record [ctx cfg action recently-sent]
  (let [{:keys [action-type params call-id]} action]
    (cond
      (= action-type :reason)
      (do (swap! ctx context/add-item :reasoning {:content (:thought params)})
          nil)

      (= action-type :wait)
      (let [secs (max 5 (min 300 (:seconds params)))]
        (println (format "[agent] WAIT %ds" secs))
        {:delay (* secs 1000)})

      (= action-type :send-message)
      (let [content (:content params)]
        (swap! ctx context/add-item :action {:action-type :send-message :params params :call-id call-id})
        (println (format "[agent] EXEC send-message (item %d)" (dec (:next-id @ctx))))
        (matrix/send-message cfg content)
        (swap! recently-sent conj content)
        (swap! recently-sent #(vec (take-last 10 %)))
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
                     (try (dispatch/execute-action {:action-type action-type
                                                    :message-id (:message-id params)
                                                    :command (:command params)
                                                    :path (:path params)})
                          (catch Exception e
                            (println (format "[agent] ERROR in %s: %s" (name action-type) (.getMessage e)))
                            {:content (str "Error: " (.getMessage e))})))
            content (trunc-result (:content result))
            duplicate? (some #(= content %) (recent-result-contents ctx 3))
            _ (when-not duplicate?
                (swap! ctx context/add-item :action-result
                  {:caused-by action-id :content content}))]
        nil))))

(defn process-turn [ctx cfg system-prompt idle-count recently-sent]
  (try
    (let [response (call-llm ctx cfg system-prompt idle-count)]
      (if-let [actions (seq (parse-tool-calls response))]
        (loop [actions actions wait-info nil productive? false]
          (if-let [action (first actions)]
            (let [result (execute-and-record ctx cfg action recently-sent)
                  productive? (or productive?
                                (and (not= :wait (:action-type action))
                                     (not= :reason (:action-type action))))]
              (recur (rest actions) (or wait-info result) productive?))
          {:delay (:delay wait-info) :productive? productive?}))
      {:delay nil :productive? false}))
    (catch Exception e
      (println (format "[agent] Turn error: %s" (.getMessage e)))
      {:delay default-delay :productive? false})))

(defn start-message-watcher [ctx cfg monitor]
  (matrix/sync-loop ctx cfg monitor))

(defn run [cfg]
  (let [_ (System/setProperty "user.dir" (or (:home-dir cfg) "/home/wayfinder"))
        ctx (atom {:items [] :next-id 0})
        system-prompt (load-system-prompt (or (:prompts-dir cfg) "prompts"))
        monitor (Object.)
        threshold (or (:compact-threshold cfg) 8000)
        target (or (:compact-target cfg) 5000)
        cooldown-ms (* (or (:compact-cooldown cfg) 120) 1000)
        last-compact (atom (System/currentTimeMillis))
        curate-interval (* (or (:curate-interval cfg) 1800) 1000)
        last-curate (atom (System/currentTimeMillis))
        idle-count (atom 0)
        recently-sent (atom [])]
    (start-message-watcher ctx cfg monitor)
    (println (format "Wayfinder agent running. Connected to Matrix. Compact threshold=%d tokens target=%d tokens cooldown=%ds curate-interval=%ds"
               threshold target (or (:compact-cooldown cfg) 120) (or (:curate-interval cfg) 1800)))
    (loop [delay default-delay]
      (let [start (System/currentTimeMillis)]
        (try
          (locking monitor (.wait monitor delay))
          (catch InterruptedException _))
        (let [item-count (count (context/fetch-context @ctx))
              needs-compact (context/needs-compact? @ctx threshold)
              elapsed-since-compact (- start @last-compact)
              can-compact (> elapsed-since-compact cooldown-ms)
              elapsed-since-curate (- start @last-curate)
              can-curate (> elapsed-since-curate curate-interval)]
          (when (and needs-compact can-compact)
            (println (format "[agent] Context at %d items (threshold %d), triggering compaction"
                       item-count threshold))
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
          (let [next-result (process-turn ctx cfg system-prompt @idle-count recently-sent)]
            (if (:productive? next-result)
              (reset! idle-count 0)
              (swap! idle-count inc))
            (dump-context ctx cfg)
            (recur (or (:delay next-result) default-delay))))))))
