(ns gelatinous-cube.api-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is use-fixtures]]
            [clojure.set :as set]
            [clojure.string :as str]
            [gelatinous-cube.api :as sut]
            [gelatinous-cube.tx-sources :as tx-sources]
            [gelatinous-cube.impl :as impl]
            [gelatinous-cube.util :as u]
            [gelatinous-cube.test-utils :as tu]
            [datomic.client.api :as d]))


(use-fixtures :each tu/test-system-fixture)


(def config (u/read-resource "sample-config.edn"))

(def all-norm-names (map :name config))
(def immutable-norm-names (map :name (remove :mutable config)))
(def mutable-norm-names (map :name (filter :mutable config)))

(def all-idents-q
  '[:find ?ident
    :where
    [?e :db/ident ?ident]])

(defn all-idents
  [conn]
  (map first
       (d/q all-idents-q (d/db conn))))

(def conformed-norm-names-q
  '[:find ?name
    :in $ ?tracking-attr
    :where
    [?e ?tracking-attr ?name]])

(defn conformed-norm-names
  [conn]
  (map first
       (d/q conformed-norm-names-q
            (d/db conn)
            sut/*tracking-attr*)))

(defn norm-idents
  [conn norm-maps]
  (let [norm-maps (impl/conform! norm-maps)
        tx-data (mapcat (partial tx-sources/tx-data-for-norm conn)
                        norm-maps)]
    (conj (keep :db/ident tx-data)
          sut/*tracking-attr*)))

(defn new-idents=
  [conn pre-existing-idents expected-new-idents]
  (= (set expected-new-idents)
     (set/difference (set (all-idents conn))
                     (set pre-existing-idents))))

(defn conformed=
  [conn expected-norm-names]
  (= (sort expected-norm-names)
     (sort (conformed-norm-names conn))))

(def immutable-norm-maps
  "`tx-fn` norms can't be pre-processed for expectation purposes below
  because they require prior norms to already have been transacted."
  (remove :mutable config))

(deftest ensure-conforms-basic
  (let [idents-before (all-idents tu/*conn*)
        expected-new-idents (norm-idents tu/*conn* immutable-norm-maps)]
    (is (= {:succeeded-norms all-norm-names}
           (sut/ensure-conforms tu/*conn* config)))
    (is (conformed= tu/*conn* all-norm-names))
    (is (new-idents= tu/*conn* idents-before expected-new-idents))))

(deftest ensure-conforms-idempotency
  (let [tx-count #(count (d/tx-range tu/*conn* {:start 0 :end 1e6}))
        tracking-attr-count 1
        conformed-tx-count (+ (count all-norm-names)
                              tracking-attr-count)
        t1-tx-count (tx-count)]
    (is (= {:succeeded-norms all-norm-names}
           (sut/ensure-conforms tu/*conn* config)))
    (let [t2-tx-count (tx-count)]
      (is (= (+ t1-tx-count conformed-tx-count)
             t2-tx-count))
      (is (= {:unneeded-norms immutable-norm-names
              :succeeded-norms mutable-norm-names}
             (sut/ensure-conforms tu/*conn* config)))
      (is (= (+ t2-tx-count (count mutable-norm-names))
             (tx-count))))))

(deftest ensure-conforms-return-shape
  (is (= {:succeeded-norms all-norm-names}
         (sut/ensure-conforms tu/*conn* config)))
  (with-redefs [impl/transact-norm (fn [& _] (throw (ex-info "!" {})))]
    (is (thrown-with-data?
         {:unneeded-norms immutable-norm-names
          :failed-norm (first mutable-norm-names)}
         (sut/ensure-conforms tu/*conn* config)))))

(deftest ensure-conforms-specified-subset-of-norms
  (let [idents-before (all-idents tu/*conn*)
        expected-new-idents (norm-idents tu/*conn*
                                         (sut/norm-maps-by-name config [:base-schema]))]
    (is (= {:succeeded-norms [:base-schema]}
           (sut/ensure-conforms tu/*conn* config [:base-schema])))
    (is (conformed= tu/*conn* [:base-schema]))
    (is (new-idents= tu/*conn* idents-before expected-new-idents))))

(deftest ensure-conforms-custom-tracking-attr
  (let [custom-attr :custom/tracking-attr]
    (binding [sut/*tracking-attr* custom-attr]
      (is (= {:succeeded-norms all-norm-names}
             (sut/ensure-conforms tu/*conn* config)))
      (is (conformed= tu/*conn* (map :name config))))))

(deftest ensure-conforms-respects-custom-tx-sources
  (let [idents-before (all-idents tu/*conn*)
        schema [{:db/ident :banans
                 :db/valueType :db.type/string
                 :db/cardinality :db.cardinality/one}]
        encode (comp str/reverse pr-str)
        decode (comp edn/read-string str/reverse)]
    (defmethod tx-sources/tx-data-for-norm :tx-banans
      [_conn {payload :tx-source}]
      (decode payload))
    (is (= {:succeeded-norms [:banans]}
           (sut/ensure-conforms tu/*conn*
                                [{:name :banans :tx-banans (encode schema)}]
                                [:banans])))
    (remove-method tx-sources/tx-data-for-norm :tx-banans)
    (is (new-idents= tu/*conn* idents-before [sut/*tracking-attr* :banans]))
    (is (conformed= tu/*conn* [:banans]))))

(deftest ensure-conforms-fails-to-validate-unknown-tx-sources
  (let [schema [{:db/ident :banans
                 :db/valueType :db.type/string
                 :db/cardinality :db.cardinality/one}]
        encode (comp str/reverse pr-str)]
    (is (thrown-with-data?
         {:cognitect.anomalies/category :cognitect.anomalies/incorrect
          :cognitect.anomalies/message "Norm config failed to validate."}
         (sut/ensure-conforms tu/*conn* [{:name :banans
                                          :tx-banans (encode schema)}])))))

(deftest ensure-conforms-fails-to-validates-malformed-norm-maps
  (let [config (assoc-in config [2 :tx-data] ["this is not a norm map"])]
    (is (thrown-with-data?
         {:cognitect.anomalies/category :cognitect.anomalies/incorrect
          :cognitect.anomalies/message "Norm config failed to validate."}
         (sut/ensure-conforms tu/*conn* config)))))

(deftest ensure-conforms-conveys-failed-norms
  (let [config (assoc-in config [2 :tx-data] [{:unknown/attribute :unknown/attribute-also}])]
    (is (thrown-with-data?
         {:succeeded-norms [:base-schema :add-user-zip]
          :failed-norm :add-user-data}
         (sut/ensure-conforms tu/*conn* config)))))