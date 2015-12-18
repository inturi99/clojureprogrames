;;(ns clojure-programes.livingclojure
(ns clojureprogrames.core
  (:gen-class))
(defn drink[x]
  (when x "x = 10"))

(defn grow[name direction]
  (if (= direction :small)
    (str name " is growing smaller")
    (str name " is growing bigger")))
;;(grow "alice" :small)

(defn toggle-grow [directions]
  (if(= directions :small) :big :small))
(defn oh-my [direction]
(str "Oh My! Your are growing " direction)
)

(defn add[x y]
  (+ x y))
(def add-5 (partial add 5))

(let [[color [size] :as original]["blue" ["Very small"]]]
  {:color color :size  size :original original})

(let [{:keys [flower1 flower2]}
      {:flower1 "red" :flower2 "blue"}]
  (str "the flowers are " flower1 " and " flower2))

(defn flowers[color]
  (str "the flowers are "
  (:flower1 color) " and "
  (:flower2 color)))

(defn flowers1 [{:keys [flower1 flower2]}]
  (str "the flowers are " flower1 " and " flower2))

(def adjs ["normal" "to small" "to big" "swimming"])
(defn alice-is[in out]
  (if(empty? in)
    out
    (alice-is (rest in)
              (conj out
                    (str "Alice is  " (first in))))))
;;(alice-is adjs[])

(defn countdown[n]
  (if(= n 0)
    n
    (recur (- n 1))))

(def animals ["mouse" "duck" "dodo" "lory" "eaglet"])

(def colors ["brown" "black" "blue"])

(defn gen-animal-string [animals colors]
  (str colors "-" animals))
;;(map gen-animal-string animals colors)

;;Reduce	the	Ultimate
(reduce	(fn	[r	x]	(+	r	(*	x	x)))	[1	2	3])

(reduce	(fn	[r	x]	(if	(nil?	x)	r	(conj	r	x)))
        []
        [:mouse	nil	:duck	nil	nil	:lory])

;;Other	Useful	Data	Shaping	Expressions
((complement	nil?)	nil)
;;------------
((complement	nil?)	1)
;;----------
(filter	(complement	nil?)	[:mouse	nil	:duck	nil])
;;---------
(filter	keyword?	[:mouse	nil	:duck	nil])
;; ------
(remove	nil?	[:mouse	nil	:duck	nil])
;; -----
(for	[animal	[:mouse	:duck	:lory]]
  (str	(name	animal)))
;;--------

(for	[animal	[:mouse	:duck	:lory]
       color		[:red	:blue]]
  (str	(name	color)	(name	animal)))

;; let
(for [animal [:mouse :duck :lory]
      color[:red :blue]
      :let [animal-str(str "animal_" (name animal))
            color-str(str "color-"(name color))
            display-str(str animal-str "-" color-str)]]
  display-str)

;; when
(for [animal [:mouse :duck :lory]
      color[:red :blue]
      :let [animal-str(str "animal_" (name animal))
            color-str(str "color-"(name color))
            display-str(str animal-str "-" color-str)]
      :when (= color :blue)]
  display-str)

;; flatten
(flatten [[:duck [:mouse] [[:lory]]]])



