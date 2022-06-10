(ns gelatinous-cube.api
  (:require [gelatinous-cube.impl :as impl]))


(def ^:dynamic *tracking-attr* :gelatinous-cube/absorbed)


(defn needed-norms
  [conn {:keys [norm-maps]}]
  (->> norm-maps
       (filter (fn [norm-map]
                 (impl/needed? conn norm-map *tracking-attr*)))
       (map :name)))

(defn absorb-norms
  [conn norm-maps]
  (reduce
   (fn [acc {norm-name :name :as norm-map}]
     (if (not (impl/needed? conn norm-map *tracking-attr*))
       (update acc :unneeded-norms (fnil conj []) norm-name)
       (try
         (let [tx-result (impl/transact-norm! conn norm-map *tracking-attr*)]
           (-> acc
               (update :succeeded-norms (fnil conj []) norm-name)
               (assoc-in [:tx-results norm-name] tx-result)))
         (catch Exception e
           (throw (ex-info "Norm failed to absorb"
                           (assoc acc :failed-norm norm-name)
                           e))))))
   {}
   norm-maps))

(defn norm-maps-by-name
  [norm-maps names]
  (filter (comp (set names) :name)
          norm-maps))

(defn absorb
  [conn {:keys [norm-maps only-norms]
         :or {only-norms (map :name norm-maps)}}]
  (let [adapted-norms (-> norm-maps
                          (norm-maps-by-name only-norms)
                          impl/adapt!)]
    (impl/ensure-tracking-schema! conn *tracking-attr*)
    (absorb-norms conn adapted-norms)))