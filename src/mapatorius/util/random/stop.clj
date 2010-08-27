(ns
  mapatorius.util.random.stop
  (:require mapatorius.util.random)
  (:require mapatorius.util.random.lfsr)
  (:use mapatorius.util.random)
  (:use mapatorius.util.random.lfsr))

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

(def example-gen (StopGen lfsr17-6 lfsr25-7))
