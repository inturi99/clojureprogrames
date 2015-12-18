(ns clojureprogrames.core
  (:gen-class))

(defn is-prime? [n]
  (empty? (filter #(= 0 (mod n  %)) (range 2 n))))

 ;;Calculates the absolute value of a number
  (defn abs [n]
   (if(< n 0)
    (* -1 n)
    n))

;;returns the average of two arguments
(defn avg [a b]
  (/(+ a b ) 2))

;;Tests if a guess is close enough to the real square root
(defn good-enough? [number guess]
  (let [diff (- (* guess guess ) number)]
    (if (< (abs diff) 0.001)
      true false )))
;;returns the square root of the supplied number
(defn sqrt
  ([number](sqrt number 1.0))
  ([number guess] 
  (if (good-enough? number guess)
    guess
    (sqrt number(avg guess(/ number guess))))))

;;Calculates a number to the power of a provided exponent.
(defn power [number exponent]
  (if (zero? exponent)  1
      (* number (power number (- exponent 1)))))

;; adds all the numbers below a given limit
(defn add-up ([limit]
  (add-up limit 0 0))
  ([limit current sum]
   (if (< limit current)
    sum
    (recur limit (+ 1 current )(+ current sum)))))

(defn printvalue[x y]
  (if (> x y) 
    (prn "max value of x = " x)
    (if (< x y ) (prn "max value of y = "  y)))
  (if(= x y)
    (prn "x = y " x "=" y)
    (prn "x != y " x "!=" y)))

(defn leapyear [x]
  (if (and (or (zero? (mod x 4)) (zero? (mod x 400))) (not= 0 (mod x 100)))
  (prn x "is a Leap Year")
  (prn x "is not a Leap Year")))

(defn leapyearc [x]
  (cond (zero? (mod x 400)) true
        (zero? (mod x 100)) false 
        :else (zero? (mod x 4))))

(def my-matcher (re-matcher #"[a-zA-z]*" "test"))

(defn printstrin[x]
   (if (> x 10)
    (do
      (prn "Success ") x)
    (do
       (prn "Failure") x))
   )

(defn printmul[x]
  (if (> x 10)
    (do
      (prn "Success " x) x)
    (do
      (prn "Failure " x) x))
  )

(defn sum-even-numbers [nums]
  (if-let [nums (seq (filter even? nums))]
    (reduce + nums)
    "No even numbers found."))

(defn gcd [x y]
  (cond
    (zero? x) y
    (zero? y) x
    :else (recur y (mod x y))))

(defn lcm [x y]
  (/ (* x y) (gcd x y)))

;;(reduce lcm '(10 20 30))

(defn groupby[f col]
  (reduce #(assoc %1 (f %2)
                  (conj (apply vector (%1 (f %2))) %2)) {} col))

(defn unique[]
  )



