(ns leihs.core.availability.changes-test
  (:require
   [clojure.test :refer :all]
   [java-time :as t]
   [leihs.core.availability.changes :as ch]))

(def morning [(t/local-time "08:00") (t/local-time "13:00")])
(def afternoon [(t/local-time "13:00") (t/local-time "18:00")])
(def in-a-week (t/plus (ch/local-date) (t/days 7)))

(defn- reservation-in [[start end]]
  {:status "approved" :end_date in-a-week :start_time start :end_time end})

(deftest time-window-test
  (is (nil? (ch/time-window {:start_time nil :end_time nil})))
  (is (= morning (ch/time-window {:start_time "08:00" :end_time "13:00"})))
  (is (= morning (ch/time-window {:start_time (java.sql.Time/valueOf "08:00:00")
                                  :end_time (java.sql.Time/valueOf "13:00:00")}))))

(deftest conflicts-with-window?-test
  (let [whole-day {:status "approved" :end_date in-a-week}]
    (is (ch/conflicts-with-window? nil (reservation-in morning)))
    (is (ch/conflicts-with-window? morning whole-day))
    (is (ch/conflicts-with-window? morning (reservation-in morning)))
    (is (not (ch/conflicts-with-window? afternoon (reservation-in morning))))
    (is (ch/conflicts-with-window? [(t/local-time "12:00") (t/local-time "14:00")]
                                   (reservation-in afternoon)))
    (testing "a late reservation blocks every window"
      (is (ch/conflicts-with-window? afternoon
                                     (assoc (reservation-in morning)
                                            :status "signed"
                                            :end_date (t/minus (ch/local-date) (t/days 1))
                                            :returned_date nil))))))

(deftest windows-to-check-test
  (let [whole-day {:status "approved" :end_date in-a-week}]
    (is (= [nil] (ch/windows-to-check nil [whole-day])))
    (is (= [morning] (ch/windows-to-check morning [whole-day])))
    (is (= [morning afternoon]
           (ch/windows-to-check nil [whole-day
                                     (reservation-in morning)
                                     (reservation-in afternoon)
                                     (reservation-in morning)])))))

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
      (is (not (ch/conflicts-with-window? afternoon handed-over (at "12:00"))))
      (is (ch/conflicts-with-window? afternoon handed-over (at "14:00"))))))
