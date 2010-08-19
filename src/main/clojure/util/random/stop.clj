(ns
  clojure.util.random.stop
  (:require clojure.util.random)
  (:require clojure.util.random.lfsr)
  (:use clojure.util.random)
  (:use clojure.util.random.lfsr))

(defstruct stop-gen :PRNG :stream :gate)

(defn StopGen [s g]
  (struct-map stop-gen
    :PRNG :Stop
    :stream s
    :gate g))

(defmethod clock-rng :Stop [s]
  (let [[b gp] (clock-rng (:gate s))]
    (if b
      (let [[_ sp] (clock-rng (:stream s))]
        [(peek-gen sp) 
          (assoc (assoc s :gate gp)
            :stream sp)])
      [(peek-gen (:stream s)) (assoc s :gate gp)])))

(def example-stop (StopGen (last (get-float inner)) (last (get-float outer))))
