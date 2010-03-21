;; The main functions for Euler

(ns euler
    (:gen-class))
    
(defn problem1
    " Add all the natural numbers below one thousand that are multiples of 3 or 5. "
    [n]
    (reduce + (filter #(or (= 0 (mod % 3)) (= 0 (mod % 5))) (range n))))

(defn fib-seq []
    " Return a lazy infinite sequence of fibonacci numbers "
    ((fn rfib [a b] (cons a (lazy-seq (rfib b (+ a b))))) 0 1))
  
(defn problem2
    " Find the sum of all the even-valued terms in the Fibonacci sequence which do not exceed four million. "
    [n]
    (reduce + (filter #(= 0 (mod % 2)) (take-while #(< % n) (fib-seq)))))

(defn prime?
    " return True if n is prime "
    [n]
    
    )
(defn problem3
    " What is the largest prime factor of the number 600851475143 ? "
    [n]
    )