(ns wayfinder.context
  (:require [clojure.string]))

(defn add-item [ctx type data]
  (let [id (:next-id ctx)]
    (-> ctx
        (update :items conj {:id id :type type :data data :salience :raw})
        (update :next-id inc))))

(defn update-item [ctx id updates]
  (assoc ctx :items
         (mapv (fn [item]
                 (if (= id (:id item))
                   (merge item updates)
                   item))
               (:items ctx))))

(defn summarize-items [ctx ids data remembered?]
  (let [last-id (apply max ids)
        other-ids (remove #(= last-id %) ids)]
    (-> ctx
        (update :items (fn [items]
                         (filterv #(not (contains? (set other-ids) (:id %))) items)))
        (update-item last-id (cond-> {:salience :summarized :data data}
                               remembered? (assoc :remembered true))))))

(defn summarize-item [ctx id data remembered?]
  (summarize-items ctx [id] data remembered?))

(defn forget-items [ctx ids]
  (let [id-set (set ids)]
    (update ctx :items (fn [items] (filterv #(not (contains? id-set (:id %))) items)))))

(defn forget-item [ctx id]
  (forget-items ctx [id]))

(defn expand-with-pairs
  "Expand a set of ids to forget so action/action-result pairs live and die
   together: a rendered transcript must never contain a tool_call without its
   role:tool answer, nor an orphan tool answer."
  [ctx ids]
  (let [id-set (set ids)
        items (:items ctx)
        results-of-forgotten (->> items
                                  (filter #(and (= :action-result (:type %))
                                                (contains? id-set (get-in % [:data :caused-by]))))
                                  (map :id))
        actions-of-forgotten (->> items
                                  (keep #(when (and (= :action-result (:type %))
                                                    (contains? id-set (:id %)))
                                           (get-in % [:data :caused-by]))))]
    (into id-set (concat results-of-forgotten actions-of-forgotten))))

(defn forget-items-with-pairs [ctx ids]
  (forget-items ctx (expand-with-pairs ctx ids)))

(defn fetch-context [ctx]
  (->> (:items ctx) (remove (comp #{:forgotten} :salience))))

(defn token-estimate [items]
  (quot (reduce + (map (comp count pr-str :data) items)) 4))

(defn needs-compact? [ctx threshold]
  (> (token-estimate (fetch-context ctx)) threshold))

(defn fetch-id [ctx id]
  (filter #(= id (:id %)) (:items ctx)))

(defn fetch-ids [ctx ids]
  (filter #(contains? (set ids) (:id %)) (:items ctx)))

;; --- Wait marker ---
;; Consecutive deliberate waits fold into one running-total item instead of
;; logging each call: the resident sees how long it has been idle without the
;; transcript filling with wait/wait/wait.

(defn live-wait-marker?
  "Is `id` a wait-marker that still exists un-summarized? False for nil and
   for markers the compactor has already touched."
  [ctx id]
  (boolean (some #(and (= id (:id %))
                       (= :wait-marker (:type %))
                       (= :raw (:salience %)))
             (:items ctx))))

(defn accrue-wait
  "Fold elapsed milliseconds into wait-marker `id`. The caller guarantees the
   marker is live (see live-wait-marker?)."
  [ctx id elapsed-ms]
  (let [item (first (fetch-id ctx id))]
    (update-item ctx id
      {:data (-> (:data item)
                 (update :elapsed-ms + elapsed-ms)
                 (update :waits inc))})))

;; --- Action ledger ---
;; An episodic done-list kept OUTSIDE the item stream: compaction, pruning and
;; summarization only ever touch :items, so this register survives them all.
;; It records that an action happened, never its payload — the digest is a
;; recognition aid, not a copy of the result.

(def ^:private default-ledger-cap 30)
(def ^:private default-ledger-arg-length 72)

(def ^:private ledger-stamp
  (java.time.format.DateTimeFormatter/ofPattern "MM-dd HH:mm"))

(defn- now-stamp []
  (.format (java.time.LocalDateTime/now) ledger-stamp))

(defn- digest-args
  "One-line, truncated digest of a tool's arguments."
  [params max-len]
  (let [s (clojure.string/replace (pr-str params) #"\s+" " ")]
    (if (> (count s) max-len)
      (str (subs s 0 max-len) "...")
      s)))

(defn record-action
  "Append a completed action to the ledger. Past the cap, the oldest entries
   are rolled into a single counted line: they lose their detail, not the fact
   that they happened."
  [ctx tool params ok? opts]
  (let [cap (max 1 (or (:cap opts) default-ledger-cap))
        entry {:at (now-stamp)
               :tool (name tool)
               :args (digest-args params (or (:arg-length opts) default-ledger-arg-length))
               :ok (boolean ok?)}
        ledger (conj (vec (:ledger ctx)) entry)]
    (if (> (count ledger) cap)
      (let [overflow (- (count ledger) cap)
            oldest (first ledger)]
        (-> ctx
            (assoc :ledger (vec (drop overflow ledger)))
            (update :ledger-rolled (fnil + 0) overflow)
            (update :ledger-rolled-since #(or % (:at oldest)))))
      (assoc ctx :ledger ledger))))

(defn- ledger-line [{:keys [at tool args ok]}]
  (format "- [%s] %s %s — %s" at tool (if ok "ok" "FAILED") args))

(defn render-ledger
  "The done-list as a fixed prompt section. Returns nil while nothing has been
   done yet."
  [ctx]
  (when-let [entries (seq (:ledger ctx))]
    (let [rolled (:ledger-rolled ctx)
          header (str "Completed actions (done-list)\n"
                      "Appended automatically after every tool call, and kept outside the context that gets "
                      "summarized or pruned — so it stays accurate however old the work is. "
                      "Check here before starting a task: if it is listed, you have already done it.")
          rollup (when (and rolled (pos? rolled))
                   (format "- ...plus %d earlier actions since %s"
                     rolled (or (:ledger-rolled-since ctx) "an earlier session")))]
      (clojure.string/join "\n"
        (concat [header] (when rollup [rollup]) (map ledger-line entries))))))
