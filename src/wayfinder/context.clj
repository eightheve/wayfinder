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

(defn summarize-item [ctx id data]
  (update-item ctx id {:salience :summarized :data data}))

(defn forget-item [ctx id]
  (update-item ctx id {:salience :forgotten}))

(defn needs-compact? [ctx budget]
  (> (count (:items ctx)) budget))

(defn fetch-context [ctx]
  (->> (:items ctx) (remove (comp #{:forgotten} :salience))))

(defn fetch-id [ctx id]
  (filter #(= id (:id %)) (:items ctx)))
