(ns leihs.core.availability.changes
  (:require
   [clojure.set :as set]
   [com.rpl.specter :as s]
   [java-time :as t]
   [leihs.core.availability.allocations :as a]
   [leihs.core.availability.queries :as q]
   [leihs.core.settings :refer [settings]]
   [leihs.core.time-zone :as time-zone]))

(def UTC-ZONE-ID (java.time.ZoneId/of "UTC"))

(defn local-date
  ([] (t/local-date (t/instant) UTC-ZONE-ID))
  ([date]
   (-> date
       t/local-date
       (.atStartOfDay UTC-ZONE-ID)
       .toLocalDate)))

(defn initial-group-quantity [tx entitlement pool-id]
  (let [max-possible-quantity (count (q/get-borrowable-items tx (:model_id entitlement) pool-id))]
    (min max-possible-quantity (:quantity entitlement))))

(defn init [tx entitlements pool-id]
  (let [entitlements-map
        (as-> entitlements <>
          (reduce #(assoc %1 (:entitlement_group_id %2) (initial-group-quantity tx %2 pool-id)) {} <>)
          (set/rename-keys <> {nil :general})
          (cond-> <> (nil? (:general <>)) (assoc :general 0)))
        initial-group-allocations
        (->> entitlements-map
             (map (fn [e-map]
                    [(first e-map) {:in-quantity (second e-map)
                                    :running-reservations []}]))
             (into {}))]
    {(local-date) initial-group-allocations}))

(def replacement-interval (t/months 1))

(defn late? [reservation]
  (and (-> reservation :returned_date nil?)
       (t/before? (-> reservation :end_date local-date)
                  (local-date))))

(defn being-maintained-until [_model date]
  date)

(defn get-unavailable-from [reservation]
  (if (:item_id reservation)
    (local-date)
    (t/max (local-date (:start_date reservation))
           (local-date))))

(defn get-unavailable-until [reservation model]
  (let [date (t/max (if (late? reservation)
                      (t/plus (local-date) replacement-interval)
                      (local-date (:end_date reservation)))
                    (local-date))]
    (cond->> date
      (> (:maintenance_period model) 0)
      (being-maintained-until model))))

(defn explode-date-range [start end]
  (->> (t/iterate t/plus start (t/days 1))
       (take-while #(not= % (t/plus end (t/days 1))))))

(defn most-recent-before-or-equal [changes date]
  (let [matching (->> changes
                      keys
                      (filter #(or (= % date) (t/before? % date))))]
    (when (seq matching)
      (apply t/max matching))))

(defn between [changes date1 date2]
  (let [most-recent-date (or (most-recent-before-or-equal changes date1)
                             date1)
        dates-between (set/intersection (set (keys changes))
                                        (set (explode-date-range most-recent-date date2)))]
    (select-keys changes dates-between)))

(defn insert-for-single-date [changes date]
  (if (get changes date)
    changes
    (let [allocs-source-date (most-recent-before-or-equal changes date)
          allocs (get changes allocs-source-date)]
      (assoc changes date allocs))))

(defn insert-for-time-span [changes date1 date2]
  (-> changes
      (insert-for-single-date date1)
      (insert-for-single-date (t/plus date2 (t/days 1)))))

(defn update-allocations [inner-changes allocated-group-id reservation]
  (let [group-alloc-path [s/MAP-VALS (s/submap [allocated-group-id]) s/MAP-VALS]]
    (->> inner-changes
         (s/transform (conj group-alloc-path :in-quantity) #(- % (:quantity reservation)))
         (s/transform (conj group-alloc-path :running-reservations) #(conj % (:id reservation)))
         (into {}))))

(defn update-inner-changes
  [changes date1 date2 reservation inventory-pool-and-model-group-ids]
  (let [inner-changes (between changes date1 date2)
        allocated-group-id (a/get-group-id reservation
                                           inner-changes
                                           inventory-pool-and-model-group-ids)]
    (merge changes
           (update-allocations inner-changes allocated-group-id reservation))))

(defn extend-with
  [changes reservation model inventory-pool-and-model-group-ids]
  (let [unavailable-from (get-unavailable-from reservation)
        unavailable-until (get-unavailable-until reservation model)]
    (-> changes
        (insert-for-time-span unavailable-from unavailable-until)
        (update-inner-changes unavailable-from
                              unavailable-until
                              reservation
                              inventory-pool-and-model-group-ids))))

(defn ->local-time [t]
  (cond
    (nil? t) nil
    (instance? java.sql.Time t) (.toLocalTime ^java.sql.Time t)
    (string? t) (t/local-time t)
    :else t))

(defn time-window
  "[start-time end-time] of a reservation by the hour (start-time of its start
  date, end-time of its end date), nil for a whole-day reservation."
  [{:keys [start_time end_time]}]
  (when (and start_time end_time)
    [(->local-time start_time) (->local-time end_time)]))

(defn- start-of-day [date]
  (t/local-date-time date (t/local-time 0)))

(defn period
  "[from to) as local date times: from start-time of start-date to end-time
  of end-date with a `window`, the whole days from start-date to end-date
  without one."
  [start-date end-date window]
  (let [start-date (local-date start-date)
        end-date (local-date end-date)]
    (if-let [[start-time end-time] window]
      [(t/local-date-time start-date start-time)
       (t/local-date-time end-date end-time)]
      [(start-of-day start-date)
       (start-of-day (t/plus end-date (t/days 1)))])))

(defn reservation-period
  "The period a reservation keeps an item from the others. With an item
  already assigned, from today on (see `get-unavailable-from`)."
  [reservation]
  (let [[from to] (period (:start_date reservation)
                          (:end_date reservation)
                          (time-window reservation))
        today (start-of-day (local-date))]
    [(if (and (:item_id reservation) (t/after? from today)) today from)
     to]))

(defn overlap? [[start1 end1] [start2 end2]]
  (and (t/before? start1 end2) (t/before? start2 end1)))

(defn now-in-time-zone
  "The current date and time in the time zone of the settings, in which the
  times of the reservations by the hour are given."
  [tx]
  (let [zone (or (-> (settings tx [:time_zone]) :time_zone time-zone/zone-id)
                 UTC-ZONE-ID)]
    (t/local-date-time (t/instant) zone)))

(defn overtime?
  "Whether a handed over reservation by the hour is still not returned after
  its end (`now` as local date time). Like a late one, the item is then
  missing for every other period."
  [now {:keys [status returned_date end_date] :as reservation}]
  (boolean
   (when-let [[_ end-time] (time-window reservation)]
     (and now
          (= "signed" (some-> status name))
          (nil? returned_date)
          (not (t/after? (t/local-date-time (t/local-date end_date) end-time) now))))))

(defn conflicts-with-period?
  "Whether the reservation competes for the same items as a request for
  `period`. Late and overtime reservations conflict with every period."
  ([period reservation] (conflicts-with-period? period reservation nil))
  ([period reservation now]
   (or (late? reservation)
       (overtime? now reservation)
       (overlap? period (reservation-period reservation)))))

(defn intervals
  "`period` split at the starts and ends of the reservations within it. In
  each part the same reservations compete for the items all the time, so a
  quantity is available for the period when it is in every part."
  [[from to] reservations]
  (let [inner (->> reservations
                   (mapcat reservation-period)
                   (filter #(and (t/after? % from) (t/before? % to))))
        bounds (->> (concat [from to] inner) distinct sort)]
    (map vector bounds (rest bounds))))

(defn- load-model-in-pool
  "The running reservations and a fn building the changes for a subset of
  them."
  [tx model-id pool-id exclude-res-ids]
  (let [model (q/get-model-by-id tx model-id)
        entitlements (q/get-entitlements-for-model-and-pool tx model-id pool-id)
        group-ids (q/get-inventory-pool-and-model-group-ids tx model-id pool-id)
        initial-changes (init tx entitlements pool-id)]
    {:running-reservations (q/running-reservations tx model-id pool-id exclude-res-ids)
     :build-fn (fn [reservations]
                 (reduce (fn [changes reservation]
                           (extend-with changes reservation model group-ids))
                         initial-changes
                         reservations))}))

(defn main-per-period
  "Changes for each part of the requested period (see `intervals`), each over
  the reservations competing in it. A quantity is available when it is
  available in all of them. `window` is [start-time end-time] (strings or
  local times) for a request by the hour, from start-time of start-date to
  end-time of end-date, nil for whole days."
  [tx model-id pool-id exclude-res-ids start-date end-date window]
  (let [{:keys [running-reservations build-fn]}
        (load-model-in-pool tx model-id pool-id exclude-res-ids)
        [from to] (period start-date end-date (some->> window (mapv ->local-time)))
        ;; the past does not matter (the changes start today)
        from (let [today (start-of-day (local-date))]
               (if (and (t/before? from today) (t/before? today to)) today from))
        now (now-in-time-zone tx)]
    (map (fn [interval]
           (build-fn (filter #(conflicts-with-period? interval % now)
                             running-reservations)))
         (intervals [from to] running-reservations))))

(defn main
  "Changes over all running reservations, regardless of their time windows."
  ([tx model-id pool-id] (main tx model-id pool-id nil))
  ([tx model-id pool-id exclude-res-ids]
   (let [{:keys [running-reservations build-fn]}
         (load-model-in-pool tx model-id pool-id exclude-res-ids)]
     (build-fn running-reservations))))
