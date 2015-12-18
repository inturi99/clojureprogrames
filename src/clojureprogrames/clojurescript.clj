(ns clojure-programes.clojurescript)
;; Functions with multiple arities
(defn myinc
  ([x] (myinc x 1))
  ([x increment ]
   (+ x increment )))

;;Variadic functions
(defn my-variadic-set
  [& params]
  (set params))

;;Short syntax for anonymous functions
(def average #(/ (%1 %2) 2))

(def average-longer (fn [a b] (/ (+ a b) 2)))

(def my-variadic-set1 #(set %&))

;;Flow control

;;Branching with if

(defn discount[quentity]
  (if (>= quentity 100)
    0.05
    0))

;;Branching with cond
(defn mypos?[x]
  (cond
    (> x 0) "positive"
    (< x 0)  "nagative"
    :else "zero"))

(defn translate-lang
  [code]
  (condp = (keyword code)
    :es "Spanish"
    :en "English"
    "Unknown"
    ))

;;Branching with case
(defn translate-lang-code [code]
  (case code
    "es" "Spanish"
    "en" "English"
    "Unknown"))

;;Looping with loop/recur
(defn recursive-function [x]
  (println "Looping with " x)
  (if (= x 2)
    (println "Done looping")
    (recur (inc x))))

;;Replacing for loops with higher-order functions

(defn sum-squares[accumulator item]
  (+ accumulator (* item item) ))

;;nil-punning
(defn print-coll[coll]
  (when (seq coll)
    (println "Saw" (first coll))
    (recur (rest coll))))

(defn reverse-compare [a b] compare b a)

(def sm(sorted-map reverse-compare :a 0 :b 1 :c 2))


(defn swap-pair [[first last]]
  [last first])


(let [[first last & more :as original] (range 10)]
    {:first first
       :last last
       :rest more
     :original original})

;;Data Types
(deftype User [firstname lastname])
(def Person (User. "krishnaRao" "Inturi" ))

(defn make-user [firstname lastname]
  (User. firstname lastname))


(defn IUser (full_name [_]))

(defrecord User [firstname lastname]
  IUser
  (full-name [_]
    (str firstname " " lastname)))

(def user (User. "krishnaRao" "Inturi"))

(defn user [firstname lastname]
  (reify IUser
    (full-name [_]
      (str firstname " " lastname))))

(def yen (user "yennefer" "of Vengerberg"))

;; specify
(def obj #js {})

(specify! obj
  IUser
  (full-name[_]
    "my full name"))

(def a {})

(def b (specify a
         IUser
         (full-name [_]
           )))




