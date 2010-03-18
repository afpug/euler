(ns problem3
  (:require [clojure.contrib.math :as math]))

(defn solve [n]
  (let [
    max-possible-prime (+ 0.5 (math/sqrt n))
    possible-primes (take-while #(< % max-possible-prime) (range 2 n))]
    (println possible-primes)))