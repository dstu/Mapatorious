(ns 
  mapatorious.util.random
  (:use clojure.contrib.monads))

(defmulti clock-rng :PRNG)
(defmulti split-gen :PRNG)

(defn get-bits [g n]
  "Provide n pseudo-random bits. Returns the bits and updated generator"
  (loop [c n
         acc [0 g]]
    (if (<= c 0)
      [(bit-shift-right (first acc) 1) (last acc)]
      (let [[b gs] (clock-rng (last acc))
            n      (bit-shift-left (first acc) 1)
            np     (if b (bit-set n 1) n)]
        (recur (dec c) [np gs])))))

(defn get-float [g]
  (let [[b gs] (get-bits g 52)]
    (- 
      (Double/longBitsToDouble (bit-or 
        (bit-shift-left 0x3ff 52)
        b))
    1)))
 
(defn nearest-2 [n]
  "Find the smallest value 2^x > n, where x is an integer" 
  (int (Math/ceil 
         (/ (Math/log n) 
            (Math/log 2)))))

;; Generate an integer in the range [0,range)
;; Uses rejection sampling. In the worst case,
;; the expected number of numbers generated is
;; equal to 2.
(defn get-int [rng]
  (fn [s]
    (let [bits (nearest-2 rng)]
      (loop [nm rng
             sg s]
        (if (>= nm rng)
          (let [x   (get-bits sg bits)
                sgp (fnext x)
                nmp (first x)]
            (recur nmp sgp))
          (list nm sg))))))

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
