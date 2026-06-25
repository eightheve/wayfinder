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

(defn summarize-item [ctx id data remembered?]
  (update-item ctx id (cond-> {:salience :summarized :data data}
                        remembered? (assoc :remembered true))))

(defn forget-item [ctx id]
  (update ctx :items (fn [items] (filterv #(not= id (:id %)) items))))

(defn fetch-context [ctx]
  (->> (:items ctx) (remove (comp #{:forgotten} :salience))))

(defn needs-compact? [ctx threshold]
  (> (count (fetch-context ctx)) threshold))

(defn fetch-id [ctx id]
  (filter #(= id (:id %)) (:items ctx)))
