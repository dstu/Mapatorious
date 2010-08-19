(ns
  clojure.util.random.alternating
  (:require clojure.util.random)
  (:require clojure.util.random.lfsr)
  (:use clojure.util.random)
  (:use clojure.util.random.lfsr))

(defstruct alt-gen :PRNG :stream1 :stream2 :gate)

(defn AltGen [s1 s2 g]
  (struct-map alt-gen
    :PRNG :Stop
    :stream1 s1
    :stream2 s2
    :gate g))

(defmethod clock-rng :Stop [s]
  (let [[b gp] (clock-rng (:gate s))
        sp     (assoc s :gate gp)]
    (let [spp (if b
                 (assoc :stream1 (last (clock-rng (:stream1 sp))))
                 (assoc :stream2 (last (clock-rng (:stream2 sp)))))]
      [(xor (peek-gen (:stream1 spp))
            (peek-gen (:stream2 spp)))
        spp])))

(def example-alt (AltGen inner inner outer))
