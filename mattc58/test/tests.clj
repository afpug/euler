;; Tests of the Euler functions

(ns tests
    (:use [clojure.test :only (deftest, is)])
    (:use euler))
 
(deftest test-problem1
    " Problem 1 "
    (let [answer (problem1 1000)]
        (println "Problem 1 = " answer)
        (is (= 233168 answer))))
    
(deftest test-problem2
    " Problem 2"
    (let [answer (problem2 4000000)]
        (println "Problem 2 = " answer)
        (is (= 4613732 answer))))
    
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
    (let [answer (problem3 600851475143)]
        (println "Problem 3 = " answer)
        (is (= 6857 answer))))
    
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
    (let [answer (problem4 3)]
        (println "Problem 4 = " answer)
        (is (= 9009 (problem4 2)))
        (is (= 906609 answer))))
    
(deftest test-problem5
    " test problem 5 "
    (let [answer (problem5 1 20)]
        (println "Problem 5 = " answer)
        (is (= 232792560 answer))))
    
(deftest test-problem6
    " test problem 6 "
    (let [answer (problem6 1 100)]
        (println "Problem 6 = " answer)
        (is (= 25164150 answer))))
    
(deftest test-problem7
    " test problem 7"
    (let [answer (problem7 10001)]
        (println "Problem 7 = " answer)
        (is (= 104743 answer))))
    
(deftest test-problem8
    " test problem 8"
    (let [answer (problem8 problem8-digits)]
        (println "Problem 8 = " answer)
        (is (= 40824 answer))))
        
(deftest test-problem9
    " test problem 9 "
    (let [answer (problem9 1000)]
        (println "Problem 9 = " answer)
        (is (= 31875000 answer))))
    

