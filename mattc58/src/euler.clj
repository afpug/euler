;; The main functions for Euler
(ns euler
    (:gen-class)
    (:use [clojure.contrib.math :only (expt, round, sqrt)]))
    
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
    (let [sq (inc (round (sqrt n)))]
        (loop [ num 2
                facs []]
            (if (>= num sq)
                (sort facs)
                (if (= 0 (mod n num))
                    (recur (inc num) (concat [(/ n num) num] facs))
                    (recur (inc num) facs))))))
                    
(defn prime-factors
    " return the prime factors for n "
    [n]
    (filter prime? (factors n)))
    
(defn problem3
    " What is the largest prime factor of the number 600851475143 ? "
    [n]
    (last (prime-factors n)))
    
(defn calc-products
    " calculate all distinct products of an n-digit number "
    [n]
    (let [nums (range (expt 10 (- n 1)) (expt 10 n))]
        (sort (distinct (mapcat concat (map (fn[j](map (fn[i](* i j)) nums)) nums))))))
        
(defn palindrome?
    " determines if a given number n is a palindrome "
    [n]
    (= (seq (str n)) (reverse (str n))))
    
(defn problem4
    " Find the largest palindrome made from the product of two 3-digit numbers. "
    [n]
    (loop [prods (reverse (calc-products n))]
        (if (palindrome? (first prods))
            (first prods)
        (recur (rest prods)))))
        
(defn prime-factorization
    " return the entire prime factorization for n "
    [n]
    (loop [ num n 
            primes (sort #(compare %2 %1) (prime-factors n)) 
            factorization []]
        (cond 
            (empty? primes)
                (if (prime? n) (cons n factorization) factorization)
            (= 0 (mod num (first primes))) 
                (recur (/ num (first primes)) primes (cons (first primes) factorization))
            :else (recur num (rest primes) factorization))))


(defn problem5
    " What is the smallest number that is evenly divisible by all of the numbers from 1 to 20? "
    [a b]
    (let [  prime-facs (map prime-factorization (range a (inc b)))
            primes (distinct (apply concat prime-facs))]
        (apply * (map #(expt (first (keys %)) (first (vals %))) 
                    (map (fn[p](hash-map p (apply max (map #(count (filter #{p} %)) prime-facs)))) primes)))))
     
(defn sum-of-squares
    " calculate the sum of squares between two numbers "
    [a b]
    (apply + (map #(expt % 2) (range a (inc b)))))
    
(defn square-of-sums
    " calculate the square of sums between two numbers "
    [a b]
    (expt (apply + (range a (inc b))) 2))
                   
(defn problem6
    " Find the difference between the sum of the squares and the square of the sum for ints between 1 and 100. "
    [a b]
    (- (square-of-sums a b) (sum-of-squares a b)))
    
(defn problem7
    " What is the 10001st prime number? "
    [n]
    (let [primes (filter #(not (nil? %)) (map #(if (prime? %) %) (range 2 200000)))]
        (nth primes (- n 1))))
        
(def problem8-digits (map #(Integer. (str %))  "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"))

(defn problem8
    " Find the greatest product of five consecutive digits in the 1000-digit number "
    [digits]
    (apply max (map #(reduce * %) (map #(take 5 (drop % digits)) (range (- (count digits) 4))))))

(defn pythagorean-triplet?
    " returns true if a*a + b*b = c * c"
    [a b c]
    (= (expt c 2) (+ (expt a 2) (expt b 2))))
    
(defn pythagorean-triplets
    " return all pythagorean triplets with a, b, c all between the ranges given "
    [min-a max-c]
    (filter #(not-empty %)
    (map (fn[a] (filter #(not-empty %)
            (map (fn[b] (filter #(not (nil? %))
                (map (fn[c] 
                    (if (and (< a b c) (pythagorean-triplet? a b c)) [a b c])) (range min-a max-c)))) 
                        (range min-a max-c)))) 
                            (range min-a max-c))))
                            
(defn problem9
    " There exists exactly one Pythagorean triplet for which a + b + c = 1000. Find the product abc."
    ; it's not perfect, missing 30 40 50 if passing in 120 as n. but it works for 1000.
    [n]
    (map #(reduce * %)
    (first (first
        (filter #(not-empty %)
            (map (fn[a] (filter #(not-empty %)
                    (map (fn[b] (filter #(and (not (nil? %)) 
                            (pythagorean-triplet? (first %) (second %) (nth % 2)))
                        (map (fn[c](if (and (< a b c) (= n (+ a b c))) [a b c])) (range 3 n)))) 
                                (range 2 (- n 1))))) 
                                    (range 1 (- n 2))))))))
                                    
(defn problem10
    " Find the sum of all the primes below two million. "
    [n]
    (reduce + (filter #(prime? %) (cons 2 (range 3 n 2)))))
    
    

