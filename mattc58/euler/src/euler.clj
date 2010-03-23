;; The main functions for Euler

(ns euler
    (:gen-class)
    (:use [clojure.contrib.math :only (round, sqrt)]))
    
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
    (if (or (= n 2) (= n 3))
        true
        (if (or (= 0 (mod n 2)) (= 0 (mod n 3)))
            false
            (let [sq (inc (round (sqrt n)))]
                (loop [num 5]
                    (if (>= num sq)
                        true
                        (if (= 0 (mod n num))
                            false
                            (recur (+ num 2)))))))))
                    
(defn factors
    " get the factors for n "
    [n]
    (loop [ num 2
            factors []]
        (if (>= num n)
            factors
            (if (= 0 (mod n num))
                (recur (inc num) (cons num factors))
                (recur (inc num) factors)))))
                    
(defn prime-factors
    " return the prime factors for n "
    [n]
    (filter prime? (factors n)))
    
(defn prime-factors-fast
    " return the prime factors for n "
    [n]
    (let [sq (inc (round (sqrt n)))]
        (filter #(not (nil? %))
            (loop [ num 5
                    factors [(if (= 0 (mod n 2)) 2) (if (= 0 (mod n 3)) 3)]]
                (if (>= num sq)
                    factors
                    (if (and (prime? num) (= 0 (mod n num)))
                        (recur (+ num 2) (cons num factors))
                        (recur (+ num 2) factors)))))))

(defn problem3
    " What is the largest prime factor of the number 600851475143 ? "
    [n]
    (first (prime-factors-fast n)))
