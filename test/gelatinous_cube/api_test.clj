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

(def absorbed-norm-names-q
  '[:find ?name
    :in $ ?tracking-attr
    :where
    [?e ?tracking-attr ?name]])

(defn absorbed-norm-names
  [conn]
  (map first
       (d/q absorbed-norm-names-q
            (d/db conn)
            sut/*tracking-attr*)))

(defn norm-idents
  [conn norm-maps]
  (let [norm-maps (impl/adapt! norm-maps)
        tx-data (mapcat (partial tx-sources/tx-data-for-norm conn {})
                        norm-maps)]
    (conj (keep :db/ident tx-data)
          sut/*tracking-attr*)))

(defn new-idents=
  [conn pre-existing-idents expected-new-idents]
  (= (set expected-new-idents)
     (set/difference (set (all-idents conn))
                     (set pre-existing-idents))))

(defn absorbed=
  [conn expected-norm-names]
  (= (sort expected-norm-names)
     (sort (absorbed-norm-names conn))))

(def immutable-norm-maps
  "`tx-fn` norms can't be pre-processed for expectation purposes below
  because they require prior norms to already have been transacted."
  (remove :mutable config))

(deftest absorb-basic
  (let [idents-before (all-idents tu/*conn*)
        expected-new-idents (norm-idents tu/*conn* immutable-norm-maps)]
    (is (tu/submap? (sut/absorb tu/*conn* {:norm-maps config})
                    {:succeeded-norms all-norm-names}))
    (is (absorbed= tu/*conn* all-norm-names))
    (is (new-idents= tu/*conn* idents-before expected-new-idents))))

(deftest absorb-idempotency
  (let [tx-count #(count (d/tx-range tu/*conn* {:start 0 :end 1e6}))
        tracking-attr-count 1
        absorbed-tx-count (+ (count all-norm-names)
                             tracking-attr-count)
        t1-tx-count (tx-count)]
    (is (tu/submap? (sut/absorb tu/*conn* {:norm-maps config})
                    {:succeeded-norms all-norm-names}))
    (let [t2-tx-count (tx-count)]
      (is (= (+ t1-tx-count absorbed-tx-count)
             t2-tx-count))
      (is (tu/submap? (sut/absorb tu/*conn* {:norm-maps config})
                      {:unneeded-norms immutable-norm-names
                       :succeeded-norms mutable-norm-names}))
      (is (= (+ t2-tx-count (count mutable-norm-names))
             (tx-count))))))

(deftest absorb-return-shape
  (is (tu/submap? (sut/absorb tu/*conn* {:norm-maps config})
                  {:succeeded-norms all-norm-names}))
  (with-redefs [impl/transact-norm! (fn [& _] (throw (ex-info "!" {})))]
    (is (thrown-with-data?
         {:unneeded-norms (->> config
                               (partition-by :mutable)
                               (first)
                               (map :name))
          :failed-norm (first mutable-norm-names)}
         (sut/absorb tu/*conn* {:norm-maps config})))))

(deftest absorb-specified-subset-of-norms
  (let [idents-before (all-idents tu/*conn*)
        expected-new-idents (norm-idents tu/*conn*
                                         (sut/norm-maps-by-name config [:base-schema]))]
    (is (tu/submap? (sut/absorb tu/*conn* {:norm-maps config
                                           :only-norms [:base-schema]})
                    {:succeeded-norms [:base-schema]}))
    (is (absorbed= tu/*conn* [:base-schema]))
    (is (new-idents= tu/*conn* idents-before expected-new-idents))))

(deftest absorb-custom-tracking-attr
  (let [custom-attr :custom/tracking-attr]
    (binding [sut/*tracking-attr* custom-attr]
      (is (tu/submap? (sut/absorb tu/*conn* {:norm-maps config})
                      {:succeeded-norms all-norm-names}))
      (is (absorbed= tu/*conn* (map :name config))))))

(deftest absorb-respects-custom-tx-sources
  (let [idents-before (all-idents tu/*conn*)
        schema [{:db/ident :banans
                 :db/valueType :db.type/string
                 :db/cardinality :db.cardinality/one}]
        encode (comp str/reverse pr-str)
        decode (comp edn/read-string str/reverse)]
    (defmethod tx-sources/tx-data-for-norm :tx-banans
      [_conn _extras {payload :tx-source}]
      (decode payload))
    (is (tu/submap? (sut/absorb tu/*conn* {:norm-maps [{:name :banans :tx-banans (encode schema)}]
                                           :only-norms [:banans]})
                    {:succeeded-norms [:banans]}))
    (remove-method tx-sources/tx-data-for-norm :tx-banans)
    (is (new-idents= tu/*conn* idents-before [sut/*tracking-attr* :banans]))
    (is (absorbed= tu/*conn* [:banans]))))

(deftest absorb-fails-to-validate-unknown-tx-sources
  (let [schema [{:db/ident :banans
                 :db/valueType :db.type/string
                 :db/cardinality :db.cardinality/one}]
        encode (comp str/reverse pr-str)]
    (is (thrown-with-data?
         {:cognitect.anomalies/category :cognitect.anomalies/incorrect
          :cognitect.anomalies/message "Norm config failed to validate."}
         (sut/absorb tu/*conn* {:norm-maps [{:name :banans
                                             :tx-banans (encode schema)}]})))))

(deftest absorb-fails-to-validates-malformed-norm-maps
  (let [config (assoc-in config [2 :tx-data] ["this is not a norm map"])]
    (is (thrown-with-data?
         {:cognitect.anomalies/category :cognitect.anomalies/incorrect
          :cognitect.anomalies/message "Norm config failed to validate."}
         (sut/absorb tu/*conn* {:norm-maps config})))))

(deftest absorb-conveys-failed-norms
  (let [config (assoc-in config [2 :tx-data] [{:unknown/attribute :unknown/attribute-also}])]
    (is (thrown-with-data?
         {:succeeded-norms [:base-schema :add-user-zip]
          :failed-norm :add-user-data}
         (sut/absorb tu/*conn* {:norm-maps config})))))

(deftest absorb-conveys-extras
  (let [extras {:country-code "+7"}
        absorb-result (sut/absorb tu/*conn* extras {:norm-maps config})
        db-after (-> absorb-result :tx-results (get (last all-norm-names)) :db-after)
        tels (map first
                  (d/q '[:find ?tel :where [_ :user/tel ?tel]]
                       db-after))]
    (is (not-empty tels))
    (is (every? #(str/starts-with? % "+7") tels))))
