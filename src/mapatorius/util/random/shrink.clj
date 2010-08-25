(ns
  mapatorius.util.random.shrink
  (:require mapatorius.util.random)
  (:require mapatorius.util.random.lfsr)
  (:use mapatorius.util.random)
  (:use mapatorius.util.random.lfsr))

(defstruct shrink-gen :PRNG :stream :gate)

(defn ShrinkGen [s g] 
  (struct-map shrink-gen 
    :PRNG :Shrink 
    :stream s 
    :gate g))

(defmethod clock-rng :Shrink [s]
  (loop [stream (:stream s)
         gate   (:gate s)]
    (let [[sb ss] (clock-rng stream)
          [gb gs] (clock-rng gate)]
      (if gb
        [sb (assoc (assoc s :gate gs) :stream ss)]
        (recur ss gs)))))

(defn rekey [s sb gb]
  (assoc 
    (assoc s :stream
      (assoc (:stream s) :state sb))
    :gate (assoc (:gate s) :state gb)))

(defmethod split-gen :Shrink [s]
  (let [[st gn] (get-bits (:length (:stream s)) s)
        [gt gp] (get-bits (:length (:gate s))   s)]
    [gp (rekey s st gt)]))

(def example-shrink (ShrinkGen inner outer))
