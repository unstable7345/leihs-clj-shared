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
    (testing "a late whole-day reservation blocks every period"
      (is (ch/conflicts-with-period? (on-a-week afternoon)
                                     (assoc whole-day
                                            :status "signed"
                                            :start_date (t/minus (ch/local-date) (t/days 2))
                                            :end_date (t/minus (ch/local-date) (t/days 1))
                                            :returned_date nil))))))

(deftest late-by-the-hour-test
  (let [today (ch/local-date)
        yesterday (t/minus today (t/days 1))
        handed-over (assoc (reservation-in morning)
                           :status "signed" :start_date today :end_date today
                           :item_id #uuid "00000000-0000-0000-0000-000000000001"
                           :returned_date nil)]
    (testing "a reservation by the hour not returned yet keeps nothing after its end"
      (is (not (ch/conflicts-with-period? (on-a-week morning)
                                          (assoc handed-over
                                                 :start_date yesterday
                                                 :end_date yesterday))))
      (is (not (ch/conflicts-with-period? (ch/period today today afternoon)
                                          handed-over))))
    (testing "but it does during its own period"
      (is (ch/conflicts-with-period?
           (ch/period today today [(t/local-time "12:00") (t/local-time "14:00")])
           handed-over)))))

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
