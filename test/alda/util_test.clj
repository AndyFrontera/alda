(ns alda.util-test
  (:require [clojure.test :refer :all]
            [alda.util :refer :all]))

(deftest parse-time-test
  (is (== 3000    (parse-time "3")))
  (is (== 1500    (parse-time "1.5")))
  (is (== 1750000 (parse-time "29:10")))
  (is (== 605900  (parse-time "10:05.9")))
  (is (== 7264000 (parse-time "02:01:04")))
  (is (== 3723300 (parse-time "01:02:03.30"))))
