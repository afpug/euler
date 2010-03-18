(defn multiple-of-3? [n]
  (= (mod n 3) 0))
  
(defn multiple-of-5? [n]
  (= (mod n 5) 0))

(defn multiple-of-3-or-5? [n]
  (or (multiple-of-3? n) (multiple-of-5? n)))

(defn sum [coll]
  (reduce + coll))

(defn solve [max]
  (sum (filter multiple-of-3-or-5? (range 1 max))))
