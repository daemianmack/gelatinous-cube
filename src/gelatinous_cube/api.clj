(ns gelatinous-cube.api
  (:require [gelatinous-cube.impl :as impl]))


(def ^:dynamic *tracking-attr* :gelatinous-cube/absorbed)


(defn absorb-norms
  [conn norm-maps]
  (reduce
   (fn [acc {name :name :as norm-map}]
     (if (not (impl/needed? conn norm-map *tracking-attr*))
       (update acc :unneeded-norms (fnil conj []) name)
       (try
         (impl/transact-norm! conn norm-map *tracking-attr*)
         (update acc :succeeded-norms (fnil conj []) name)
         (catch Exception e
           (throw (ex-info "Norm failed to absorb"
                           (assoc acc :failed-norm name)
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