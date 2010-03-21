;; Tests of the Euler functions

(ns tests
    (:use [clojure.test :only (deftest, is)])
    (:use euler))
 
(deftest test-problem1
    " Problem 1 "
    (println "Problem 1 = " (problem1 1000))
    (is (= 233168 (problem1 1000))))
    
(deftest test-problem2
    " Problem 2"
    (println "Problem 2 = " (problem2 4000000))
    (is (= 4613732 (problem2 4000000))))
    
(deftest test-problem3
    " Problem 3"
    (println "Problem 3 = 6857" (problem3 600851475143))
    (is (= 6857 (problem3 600851475143))))

