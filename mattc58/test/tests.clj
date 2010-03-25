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
    
(deftest test-calc-products
    " testing calc products "
    (is (some #{81} (calc-products 1)))
    (is (some #{20} (calc-products 1)))
    (is (some #{7452} (calc-products 2)))
    (is (not (some #{81} (calc-products 2)))))
    
(deftest test-palindrome?
    " test that the palindrome tester works "
    (is (palindrome? 900009))
    (is (palindrome? 99))
    (is (palindrome? 99499))
    (is (palindrome? 98766789))
    (is (palindrome? 78387))
    (is (not (palindrome? 98)))
    (is (not (palindrome? 9877891)))
    (is (not (palindrome? 7811287))))
    
(deftest test-problem4
    " test problem4 "
    (println "Problem 4 = " (problem4 3))
    (is (= 9009 (problem4 2)))
    (is (= 906609 (problem4 3))))
    
(deftest test-problem5
    " test problem 5 "
    (println "Problem 5 = " (problem5 1 20))
    (is (= 232792560 (problem5 1 20))))
    
(deftest test-problem6
    " test problem 6 "
    (println "Problem 6 = " (problem6 1 100))
    (is (= 25164150 (problem6 1 100))))
    

