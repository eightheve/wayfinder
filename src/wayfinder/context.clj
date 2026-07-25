(ns wayfinder.context)

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
