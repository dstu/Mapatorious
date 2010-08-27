(ns
  mapatorius.util.random.alternating
  (:require mapatorius.util.random)
  (:require mapatorius.util.random.lfsr)
  (:use mapatorius.util.random)
  (:use mapatorius.util.random.lfsr))

(defstruct alt-gen :PRNG :stream1 :stream2 :gate)

(defn AltGen [s1 s2 g]
  (struct-map alt-gen
    :PRNG :Alternating
    :stream1 s1
    :stream2 s2
    :gate g))

(defmethod clock-rng :Alternating [s]
  (let [[b gp] (clock-rng (:gate s))
        sp     (assoc s :gate gp)]
    (let [spp (if b
                 (assoc sp :stream1 (last (clock-rng (:stream1 sp))))
                 (assoc sp :stream2 (last (clock-rng (:stream2 sp)))))]
      [(xor (peek-gen (:stream1 spp))
            (peek-gen (:stream2 spp)))
        spp])))

(def example-gen (AltGen lfsr31-24 lfsr17-6 lfsr25-7))
