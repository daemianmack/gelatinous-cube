(ns sample-tx-fns.add-country-codes
  (:require [datomic.client.api :as d]))


(defn migrate [conn {:keys [country-code] :or {country-code "+1"}}]
  (into []
        (map (fn [[user-eid tel]]
               [:db/add user-eid :user/tel (str country-code "-" tel)]))
        (d/q '[:find ?user ?tel
               :where [?user :user/tel ?tel]]
             (d/db conn))))
