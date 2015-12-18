(ns clojureprogrames.core
  (:gen-class)
  (:use [clojure.string :only
         [lower-case split blank? split-lines]])
  (:use [clojure.java.io :only [file]]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
(defn add[a b] (+ a b))

(defn mul[a b] (* a b))

(defn factorial [n]
  (loop [current n
         next (dec current)
         total 1]
    (if (> current 1)
      (recur next (dec next) (* total current))
      total)))

(defn filter1 [f coll]
  (into []
        (apply concat
               (map
                (fn [x]
                  (if (f x) [x] []))
                coll))))

(defn prime? [n]
  (if (even? n) false
      (let [root (num (int (Math/sqrt n)))]
        (loop [i 3]
          (if (> i root) true
              (if (zero? (mod n i)) false
                  (recur (+ i 2))))))))

(defn n-primes [n]
  (loop [num 2 p []]
    (if (>= (count p) n) p
        (recur (inc num) (if (prime? num) (concat p [num]) p)))))

(defn is-prime? [n]
  (empty? (filter #(= 0 (mod n  %)) (range 2 n))))

(defn nth-prime [n]
  (take n (filter #(is-prime? %) (iterate inc 2))))

(defn my-app [f coll]
  (reduce (fn [output element]
            (conj output(f element)))
          []
          coll))

(defn my-take-while [f coll]
  (reduce (fn [out elem]
            (if (f elem)
              (conj out elem)
              (reduced out)))
          []
          coll))

(def infseq(map inc (iterate inc 0)))

(def	adjs	["normal"
                 "too	small"
                 "too	big"
                 "is	swimming"])

(defn	alice-is	[input]
  (loop	[in	input
         out	[]]
    (if	(empty?	in)
      out
      (recur	(rest	in)
                (conj	out
                        (str	"Alice	is	"	(first	in)))))))

(defn count-lines [s]
  (reduce (fn [cnt ch]
            (if (= ch \newline)
              (inc cnt)
              cnt))
          0
          s))

(defn swap [s]
  (into (empty s)
        (interleave
         (take-nth 2(drop 1 s))
         (take-nth 2 s))))

(defn is-even?[n]
  (if (= n 0)
    true
    (not (is-even?(dec n)))))

(defn greet [someone]
  (format "Hello, %s!" someone))

(defn greet1 [name]
  (str "Hello, " name "!"))

(defn nil-key? [k m]
  (nil? (m k false?)))

(defn max1 [x y z]
  (cond
    (> x y z) x
    (> y z)   y
    :else z))

;; the second to last element from a sequence
(defn sen-last [x]
  (nth x (- (count x) 2)))

;; Nth element from a sequence
(defn nth-elem [s x]
  (last (take (inc x) s)))
;;(= (nth-elem '(4 5 6 7) 2) 6)

;;count a sequence
(defn count-a-seq[c]
  (reduce (fn[x y](+ x 1)) 0 c))

;; sum it all up
(defn sum-it-all [x]
  (reduce + x))

;; Find The odd Numbers
(defn odd-numbers [x]
  (filter #(not= 0 (rem %1 2)) x))

(defn odd-numbers1 [x]
  (filter odd? x))

;;Reverse a Sequence
(defn reverse1 [x]
  (reverse (seq x)))

(defn reverse2 [x]
  (into '() x))

;; Palindrome
(defn palindrome [x]
  (= (seq x) (reverse (seq x))))

(defn palindrome1 [x]
  (if(string? x)(= x (apply str (reverse x)))
     (= x (reverse x ))))

;;Fibonacci
(defn fibonacci [n]
  (let [fib (fn fib* [a b] (cons a (lazy-seq (fib* b (+ b a)))))]
    (take n (fib 1 1))))

;;Max-Value
(defn maxv [x & xs]
  (reduce #(if(< %1 %2) %2 %1) (flatten (cons x xs))))

;;Get the Caps
(defn get-caps[x]
  (clojure.string/join ""(filter #(Character/isUpperCase %1)(seq x))))

(defn get-caps1[x]
  (apply str (map char (filter #(and (<= 65 %) (<= % 90)) (map int x)))))

;; Duplicate a Sequence
(defn duplicate [s]
  (let [f*
        (fn [acc s*]
          (conj acc s* s*))]
    (reduce f* [] s)))

;;Is Perfect
(defn divisors [n]
  (filter #(= (rem n %) 0) (range 1 (inc (/ n 2 )))))

(defn sum [s]
  (reduce + s))

(defn perfect? [n]
  (= n (sum (divisors n))))

;; Character Count
(defn char-count [str]
  (loop [counts {}
         s str]
    (if-not (empty? s)
      (let [c (first s)]
        (recur (assoc counts c (inc (get counts c 0)))
               (rest s)))
      counts)))

;; Word Count
(defn word-count [str]
  (count (clojure.string/split str #"\s+"))
  )

(defn word-frequency  [str]
  (let [words (re-seq #"\w+" (clojure.string/lower-case  str))]
    (frequencies words)))

;; day-of-week
(defn date [year month day]
  (let [m (+ (mod (+ month 9) 12) 3)
        y (- year (quot (- m month) 12))
        J (quot y 100)
        K (mod y 100)
        q day]
    (mod (+ q
            (quot (* 26 (inc m)) 10)
            K
            (quot K 4)
            (quot J 4)
            (* 5 J))
         7)))

;;(defn shift0 [m n i]
;;  (+ (mod (+ i (- n m)) n) m))
(defn shift0 [i]
  (+ (mod (+ i 6) 7) 1))

(defn day-of-week [year month day]
  (nth (.getWeekdays (java.text.DateFormatSymbols.))
       (shift0 (date year month day))))

(defn grep
  "Filters elements of coll by a regular expression.  The String
  representation (with str) of each element is tested with re-find."
  [re coll]
  (filter (fn [x] (re-find re (str x))) coll))

(defn seconds-to-week[seconds]
  (let[minutes (/ seconds 60)
       hours (/ minutes 60)
       days (/ hours 24)
       weeks (/ days 7)]
    weeks))
