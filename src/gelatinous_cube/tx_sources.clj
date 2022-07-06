(ns gelatinous-cube.tx-sources
  (:require [gelatinous-cube.util :as u]))


(defn dispatch
  [_conn _extras norm-map]
  (:source-type norm-map))

(defmulti tx-data-for-norm dispatch)

(defmethod tx-data-for-norm :tx-data tx-data
  [_ _ norm-map]
  (:tx-source norm-map))

(defmethod tx-data-for-norm :tx-resource tx-resource
  [_ _ norm-map]
  (-> norm-map :tx-source u/read-resource))

(defn eval-tx-fn
  [conn extras tx-fn]
  (try (let [resolved-tx-fn (requiring-resolve tx-fn)]
         (if (->> resolved-tx-fn meta :arglists (some #(or (= '& (first %)) (< 1 (count %)))))
           (resolved-tx-fn conn extras)
           (resolved-tx-fn conn)))
       (catch Throwable t
         (throw (ex-info (str "Exception evaluating " tx-fn)
                         {:exception t})))))

(defmethod tx-data-for-norm :tx-fn tx-fn
  [conn extras norm-map]
  (->> norm-map :tx-source (eval-tx-fn conn extras)))


(defn reorg-tx-sources
  "If a key in `norm-map` has a method implementation in
  `tx-data-for-norm`, relocate it to the val of a `:source-type` key,
  with a sibling `:tx-source` key/val. That is, given a `norm-map`
  of...

    {:name :some-name
     :s3-file 's3://some-bucket/some-key'}

  return...

    {:name :some-name
     :source-type :s3-file
     :tx-source 's3://some-bucket/some-key'}

  This allows the user's config to reference novel tx sources with
  dispatchable values at predictable coordinates while keeping config
  syntax as terse as possible.

  If no key in `norm-map` has such a method impl, declare as invalid."
  [norm-map]
  (let [meths   (methods tx-data-for-norm)
        reorged (reduce-kv
                 (fn [acc k v]
                   (if (contains? meths k)
                     (reduced (-> acc
                                  (assoc :source-type k :tx-source v)
                                  (dissoc k)))
                     acc))
                 norm-map
                 norm-map)]
    (if-not (:tx-source reorged)
      :clojure.spec.alpha/invalid
      reorged)))
