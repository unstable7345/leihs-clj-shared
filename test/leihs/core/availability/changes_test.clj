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
