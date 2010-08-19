(ns 
  clojure.util.random
  (:use clojure.contrib.monads))

(defmulti get-int   :PRNG)
(defmulti get-bits  :PRNG)
(defmulti get-float :PRNG)
(defmulti clock-rng :PRNG)
(defmulti split-gen :PRNG)
(defmulti peek-gen  :PRNG)

(defmethod get-bits :default [n g]
  "Generate n pseudo-random bits."
  (loop [c n
         acc [0 g]]
    (if (<= c 0)
      [(bit-shift-right (first acc) 1) (last acc)]
      (let [[b gs] (clock-rng (last acc))
            n      (bit-shift-left (first acc) 1)
            np     (if b (bit-set n 1) n)]
        (recur (dec c) [np gs])))))

(defmethod get-float :default [g]
  "Generate a pseudo-random float."
  (let [[b gs] (get-bits 52 g)]
    [(- 
      (Double/longBitsToDouble (bit-or 
        0x3ff0000000000
        b))
    1) gs]))
 
;; Generate an integer in the range [0,range)
;; Uses rejection sampling. In the worst case,
;; the expected number of numbers generated is
;; equal to 2.
(defmethod get-int :default [n g]
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
