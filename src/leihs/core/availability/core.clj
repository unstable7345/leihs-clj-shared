(ns leihs.core.availability.core
  (:require
   [leihs.core.availability.changes :as ch]
   [leihs.core.availability.queries :as q]))

(defn summed-for-groups
  "Minimum over the changes of the summed in-quantities of the groups."
  [changes group-ids]
  (->> (vals changes)
       (map (fn [allocs]
              (->> (select-keys allocs group-ids)
                   vals
                   (map :in-quantity)
                   (apply +))))
       (apply min)))

(defn maximum-available-in-pool-and-period-summed-for-groups
  "Returns the maximum available quantity for a model in a single inventory pool
  over the given date range, summed across all entitlement groups the user belongs to.
  `window` is the [start-time end-time] of a reservation by the hour (from
  start-time of start-date to end-time of end-date), nil for whole days."
  ([tx model-id user-id start-date end-date pool-id]
   (maximum-available-in-pool-and-period-summed-for-groups
    tx model-id user-id start-date end-date pool-id nil))

  ([tx model-id user-id start-date end-date pool-id exclude-res-ids]
   (maximum-available-in-pool-and-period-summed-for-groups
    tx model-id user-id start-date end-date pool-id exclude-res-ids nil))

  ([tx model-id user-id start-date end-date pool-id exclude-res-ids window]
   (let [user-group-ids (q/get-user-group-ids tx user-id)
         group-ids (concat [:general] user-group-ids)]
     (->> (ch/main-per-period tx model-id pool-id exclude-res-ids
                               start-date end-date window)
          (map (fn [changes]
                 (let [inner-changes (ch/between changes
                                                 (ch/local-date start-date)
                                                 (ch/local-date end-date))]
                   (if (empty? inner-changes)
                     0
                     (max 0 (summed-for-groups inner-changes group-ids))))))
          (apply min)))))

(defn maximum-available-in-period-summed-for-groups
  "Returns the total maximum available quantity for a model across multiple inventory pools."
  ([tx model-id user-id start-date end-date pool-ids]
   (maximum-available-in-period-summed-for-groups
    tx model-id user-id start-date end-date pool-ids nil))

  ([tx model-id user-id start-date end-date pool-ids exclude-res-ids]
   (maximum-available-in-period-summed-for-groups
    tx model-id user-id start-date end-date pool-ids exclude-res-ids nil))

  ([tx model-id user-id start-date end-date pool-ids exclude-res-ids window]
   (->> pool-ids
        (map #(maximum-available-in-pool-and-period-summed-for-groups
               tx model-id user-id start-date end-date % exclude-res-ids window))
        (apply +))))
