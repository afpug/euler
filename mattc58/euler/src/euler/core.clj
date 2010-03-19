;; The main functions for Euler

(ns euler.core
    (:gen-class))
    
(defn problem1
    " Add all the natural numbers below one thousand that are multiples of 3 or 5. "
    []
    (reduce + (filter #(or (= 0 (mod % 3)) (= 0 (mod % 5))) (range 1000))))
