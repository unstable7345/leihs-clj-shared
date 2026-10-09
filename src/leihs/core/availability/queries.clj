(ns leihs.core.availability.queries
  (:require
   [honey.sql :refer [format] :rename {format sql-format}]
   [honey.sql.helpers :as sql]
   [leihs.core.settings :refer [settings]]
   [next.jdbc.sql :refer [query] :rename {query jdbc-query}]))

(defn running-reservations [tx model-id pool-id exclude-res-ids]
  (let [timeout-minutes (-> (settings tx [:timeout_minutes])
                            :timeout_minutes
                            (or 0)
                            int)]
    (-> (sql/select :reservations.id
                    :reservations.inventory_pool_id
                    :reservations.model_id
                    :reservations.item_id
                    :reservations.quantity
                    :reservations.start_date
                    :reservations.end_date
                    :reservations.start_time
                    :reservations.end_time
                    :reservations.returned_date
                    :reservations.status
                    [[:'ARRAY (-> (sql/select :egu.entitlement_group_id)
                                  (sql/from [:entitlement_groups_users :egu])
                                  (sql/join [:entitlement_groups :eg]
                                            [:= :eg.id :egu.entitlement_group_id])
                                  (sql/where [:= :egu.user_id :reservations.user_id])
                                  (sql/order-by [:eg.name :asc]))]
                     :user_group_ids])
        (sql/from :reservations)
        (sql/left-join :items [:= :reservations.item_id :items.id])
        (sql/where [:= :reservations.type "ItemLine"])
        (cond-> pool-id (sql/where [:= :reservations.inventory_pool_id pool-id]))
        (sql/where [:or [:is-null :reservations.item_id] [:= :items.is_borrowable true]])
        (sql/where [:not-in :reservations.status ["draft" "rejected" "canceled" "closed"]])
        (sql/where [:not [:and
                          [:= :reservations.status "unsubmitted"]
                          [:< :reservations.updated_at
                           [:raw (format "now() at time zone 'UTC' - interval '%d minutes'"
                                         timeout-minutes)]]]])
        (sql/where [:not [:and
                          [:< :reservations.end_date [:raw "(now() at time zone 'UTC')::date"]]
                          [:is-null :reservations.item_id]]])
        (sql/where [:= :reservations.model_id model-id])
        (cond-> (not (empty? exclude-res-ids))
          (sql/where [:not-in :reservations.id exclude-res-ids]))
        (sql/order-by [:reservations.created_at :asc])
        sql-format
        (->> (jdbc-query tx)))))

(defn get-entitlements-for-model-and-pool [tx model-id pool-id]
  (let [named-groups (-> (sql/select :entitlements.model_id
                                     [:entitlement_groups.inventory_pool_id :inventory_pool_id]
                                     :entitlements.entitlement_group_id
                                     :entitlements.quantity)
                         (sql/from :entitlements)
                         (sql/join :entitlement_groups
                                   [:= :entitlement_groups.id :entitlements.entitlement_group_id])
                         (sql/where [:= :entitlements.model_id model-id])
                         (sql/where [:= :entitlement_groups.inventory_pool_id pool-id]))
        general-groups (-> (sql/select :i.model_id
                                       :i.inventory_pool_id
                                       [nil :entitlement_group_id]
                                       [[:- [:count :i.id]
                                         [:coalesce
                                          (-> (sql/select [[:sum :es.quantity] :s])
                                              (sql/from [:entitlements :es])
                                              (sql/join :entitlement_groups
                                                        [:= :entitlement_groups.id :es.entitlement_group_id])
                                              (sql/where [:= :es.model_id :i.model_id])
                                              (sql/where [:= :entitlement_groups.inventory_pool_id :i.inventory_pool_id])
                                              (sql/group-by :entitlement_groups.inventory_pool_id :es.model_id))
                                          0]]
                                        :quantity])
                           (sql/from [:items :i])
                           (sql/where [:= :i.retired nil])
                           (sql/where [:= :i.is_borrowable true])
                           (sql/where [:= :i.parent_id nil])
                           (sql/where [:= :i.model_id model-id])
                           (sql/where [:= :i.inventory_pool_id pool-id])
                           (sql/group-by :i.inventory_pool_id :i.model_id)
                           sql-format
                           (->> (jdbc-query tx)))]
    (concat
     (-> named-groups sql-format (->> (jdbc-query tx)))
     general-groups)))

(defn get-inventory-pool-and-model-group-ids [tx model-id pool-id]
  (-> (sql/select :entitlements.entitlement_group_id)
      (sql/from :entitlements)
      (sql/join :entitlement_groups
                [:= :entitlement_groups.id :entitlements.entitlement_group_id])
      (sql/where [:= :entitlements.model_id model-id])
      (sql/where [:= :entitlement_groups.inventory_pool_id pool-id])
      (sql/order-by [:entitlement_groups.name :asc])
      sql-format
      (->> (jdbc-query tx)
           (map :entitlement_group_id))))

(defn get-user-group-ids [tx user-id]
  (-> (sql/select :entitlement_group_id)
      (sql/from :entitlement_groups_users_unified)
      (sql/where [:= :user_id user-id])
      sql-format
      (->> (jdbc-query tx)
           (map :entitlement_group_id))))

(defn get-model-by-id [tx id]
  (-> (sql/select :*)
      (sql/from :models)
      (sql/where [:= :id id])
      sql-format
      (->> (jdbc-query tx))
      first))

(defn get-borrowable-items [tx model-id pool-id]
  (-> (sql/select :*)
      (sql/from :items)
      (sql/where [:= :model_id model-id])
      (sql/where [:= :inventory_pool_id pool-id])
      (sql/where [:= :retired nil])
      (sql/where [:= :is_borrowable true])
      (sql/where [:= :parent_id nil])
      sql-format
      (->> (jdbc-query tx))))
