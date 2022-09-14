(ns gelatinous-cube.specs
  (:require [clojure.spec.alpha :as s]
            [gelatinous-cube.tx-sources :as tx-sources]))

(s/def ::op #{:db/add :db/retract})

(s/def ::vector-form (s/cat :op ::op
                            :n-identifier some? ;; More granular?
                            :identifier keyword?
                            :value some?))

(s/def ::tx-data-form (s/nonconforming (s/or :map map? :vector ::vector-form)))
(s/def ::tx-data (s/coll-of ::tx-data-form))
(s/def ::tx-fn symbol?)
(s/def ::tx-resource string?)

(s/def ::mutable boolean?)
(s/def ::name keyword?)

(s/def ::norm-map
  (s/and (s/keys :req-un [::name]
                 :opt-un [::tx-data
                          ::tx-resource
                          ::tx-fn
                          ::mutable])
         (s/conformer tx-sources/reorg-tx-sources)))

(s/def ::norm-maps
  (s/coll-of ::norm-map))