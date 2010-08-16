(ns 
  mapatorious.util.random
  (:use clojure.contrib.monads))

;; Namespace is lamed up. Stu -- can you fix this, plz?
(load-file "shrink.clj")

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

(def uniform
  (domonad random-m
    [x (get-int 256)]
     x))

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

(println (take 100 (value-seq (sample-until #(>= % 100) (get-int 101)) exsg)))


