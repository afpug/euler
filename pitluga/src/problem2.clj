(def fibonacci
  (lazy-cat [1 2] (map + fibonacci (rest fibonacci))))

(defn sum [coll]
  (reduce + coll))

(defn solve [max]
  (sum (filter even? (take-while #(< % max) fibonacci))))