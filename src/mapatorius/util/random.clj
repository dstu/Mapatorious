(ns 
  mapatorius.util.random
  (:use clojure.contrib.monads))

(defmulti get-int    :PRNG)
(defmulti get-bits   :PRNG)
(defmulti get-double :PRNG)
(defmulti get-normal :PRNG)
(defmulti clock-rng  :PRNG)
(defmulti split-gen  :PRNG)
(defmulti peek-gen   :PRNG)

(defmethod get-bits :default [n g]
  "Return n pseudo-random bits and an updated generator."
  (loop [c n
         acc [0 g]]
    (if (<= c 0)
      [(bit-shift-right (first acc) 1) (last acc)]
      (let [[b gs] (clock-rng (last acc))
            n      (bit-shift-left (first acc) 1)
            np     (if b (bit-set n 1) n)]
        (recur (dec c) [np gs])))))


;; Generate pseudo-random uniformly distributed floats [0,1)
;; This uses the method described by Saito & Matsumoto
;; at MCQMC'08.
;; We generate a 52-bit significand.
;; The exponential part is a constant that puts us on the interval [1,2)
;; The result is then shifted to the interval [0,1).

(defmethod get-double :default [g]
  "Returns a uniformly distributed IEEE float and an updated generator."
  (let [[b gs] (get-bits 52 g)]
    [(- 
      (Double/longBitsToDouble (bit-or 
        0x3ff0000000000000
        b))
    1) gs]))

;; Generate pseudo-random normally distributed floats
;; mean = 0, stdev = 1
;; Uses the Box-Muller transform. 
;; We could generate two numbers, but it clutters the interface.
;; There's probably a good solution to this.

(defmethod get-normal :default [g]
  "Returns a normally distributed IEEE float and an updated generator."
  (let [[a g1] (get-double g)
        [b g2] (get-double g1)
        n  (Math/sqrt (* -2 (Math/log a)))]
    (* n (Math/cos (* 2 Math/PI b)))))
      
;; Generates an integer in the range [0,range)
;; using rejection sampling. In the worst case,
;; the expected number of numbers generated is
;; equal to 2.

(defmethod get-int :default [n g]
    "Returns a uniformly distributed integer and an updated generator."
    (let [bit-count (int (Math/ceil (/ (Math/log n) (Math/log 2))))]
      (loop [nm n
             gs g]
        (if (>= nm n)
          (let [x   (get-bits gs bit-count)
                gsp (fnext x)
                nmp (first x)]
            (recur nmp gsp))
          (list nm gs)))))

(defmonad random-m
  [m-result     (fn m-result-random [v]
                  (fn [r] [v r]))
   m-bind       (fn m-bind-random [mv f]
                  (fn [s]
                    (let [[v ss] (mv s)]
                      ((f v) ss))))
  ])

(defn value-seq [f seed]
  (lazy-seq
    (let [[value next] (f seed)]
     (cons value (value-seq f next)))))

(defn update-rng [f] (fn [s] [s (f s)]))

(defn set-rng [s] (update-state (fn [_] s)))

(def seed
  (domonad random-m
    [s split-gen]
     s))

;; As unsatisfying as this code is, it worked on our first try.
(defn sample-until [predicate rv]
  (domonad random-m
    [gen split-gen]
    (loop [g gen]
      (let [n (rv g)]
        (if (predicate (first n))
            (first n)
            (recur (second n)))))))

(defn generate-numbers [rv]
  (domonad random-m
    [n rv]
     n))
