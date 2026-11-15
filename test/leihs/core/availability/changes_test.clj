(ns leihs.core.availability.changes-test
  (:require
   [clojure.test :refer :all]
   [java-time :as t]
   [leihs.core.availability.changes :as ch]))

(def morning [(t/local-time "08:00") (t/local-time "13:00")])
(def afternoon [(t/local-time "13:00") (t/local-time "18:00")])
(def in-a-week (t/plus (ch/local-date) (t/days 7)))

(defn- reservation-in [[start end]]
  {:status "approved" :start_date in-a-week :end_date in-a-week
   :start_time start :end_time end})

(deftest time-window-test
  (is (nil? (ch/time-window {:start_time nil :end_time nil})))
  (is (= morning (ch/time-window {:start_time "08:00" :end_time "13:00"})))
  (is (= morning (ch/time-window {:start_time (java.sql.Time/valueOf "08:00:00")
                                  :end_time (java.sql.Time/valueOf "13:00:00")}))))

(defn- on-a-week [window] (ch/period in-a-week in-a-week window))

(deftest conflicts-with-period?-test
  (let [whole-day {:status "approved" :start_date in-a-week :end_date in-a-week}]
    (is (ch/conflicts-with-period? (on-a-week nil) (reservation-in morning)))
    (is (ch/conflicts-with-period? (on-a-week morning) whole-day))
    (is (ch/conflicts-with-period? (on-a-week morning) (reservation-in morning)))
    (is (not (ch/conflicts-with-period? (on-a-week afternoon) (reservation-in morning))))
    (is (ch/conflicts-with-period? (on-a-week [(t/local-time "12:00") (t/local-time "14:00")])
                                   (reservation-in afternoon)))
    (testing "a late reservation blocks every period"
      (is (ch/conflicts-with-period? (on-a-week afternoon)
                                     (assoc (reservation-in morning)
                                            :status "signed"
                                            :start_date (t/minus (ch/local-date) (t/days 2))
                                            :end_date (t/minus (ch/local-date) (t/days 1))
                                            :returned_date nil))))))

(deftest overtime?-test
  (let [today (t/local-date)
        at (fn [hh-mm] (t/local-date-time today (t/local-time hh-mm)))
        handed-over (assoc (reservation-in morning)
                           :status "signed" :start_date today :end_date today
                           :returned_date nil)]
    (is (not (ch/overtime? (at "12:00") handed-over)))
    (is (ch/overtime? (at "13:00") handed-over))
    (is (ch/overtime? (at "14:00") handed-over))
    (is (not (ch/overtime? (at "14:00") (assoc handed-over :status "approved"))))
    (is (not (ch/overtime? (at "14:00") (assoc handed-over :returned_date today))))
    (is (not (ch/overtime? nil handed-over)))
    (testing "an overtime reservation blocks the later windows of the day"
      (let [afternoon-today (ch/period today today afternoon)]
        (is (not (ch/conflicts-with-period? afternoon-today handed-over (at "12:00"))))
        (is (ch/conflicts-with-period? afternoon-today handed-over (at "14:00")))))))

(deftest assigned-item-test
  (let [today (ch/local-date)
        tomorrow (t/plus today (t/days 1))
        start-of-today (t/local-date-time today (t/local-time "00:00"))
        afternoon-tomorrow (assoc (reservation-in afternoon)
                                  :start_date tomorrow :end_date tomorrow
                                  :item_id #uuid "00000000-0000-0000-0000-000000000001")
        morning-tomorrow (ch/period tomorrow tomorrow morning)]
    (testing "an assigned item not handed over is kept just for its own period"
      (is (= tomorrow (ch/get-unavailable-from afternoon-tomorrow)))
      (is (= (t/local-date-time tomorrow (t/local-time "13:00"))
             (first (ch/reservation-period afternoon-tomorrow))))
      (is (not (ch/conflicts-with-period? morning-tomorrow afternoon-tomorrow))))
    (testing "a handed over item is missing from today on"
      (let [handed-over (assoc afternoon-tomorrow :status "signed")]
        (is (= today (ch/get-unavailable-from handed-over)))
        (is (= start-of-today (first (ch/reservation-period handed-over))))
        (is (ch/conflicts-with-period? morning-tomorrow handed-over))))))
