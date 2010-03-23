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
    
(deftest test-prime?
    " test that our prime tester works "
    (is (prime? 2267))
    (is (prime? 487))
    (is (prime? 3461))
    (is (prime? 3779))
    (is (prime? 2))
    (is (prime? 3))
    (is (prime? 5))
    (is (not (prime? 99)))
    (is (not (prime? 108)))
    (is (not (prime? 3791))))
    
(deftest test-problem3
    " Problem 3"
    (println "Problem 3 = " (problem3 600851475143))
    (is (= 6857 (problem3 600851475143))))

