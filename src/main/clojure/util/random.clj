(ns 
  mapatorious.util.random
  (:use clojure.contrib.monads))

;; Namespace is lamed up. Stu -- can you fix this, plz?
(load-file "shrink.clj")
(load-file "hull.clj")

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

(defn random-point [bound]
  (let [x0 (nth bound 0)
        x1 (nth bound 1)
        y0 (nth bound 2)
        y1 (nth bound 3)]
    (domonad random-m
      [x (get-int (- x1 x0))
       y (get-int (- y1 y0))]
       (struct point 
         (+ x x0)
         (+ y y0)))))
        
        

;;(println (take 100 (value-seq (sample-until #(>= % 100) (get-int 101)) exsg)))
(println (take 10 (value-seq (random-point (bounding-box example-hull)) exsg)))
