;; Code duplication until namespaces work
(defmulti clock-rng :PRNG)
(defstruct lfsr :PRNG :polynomial :state :length)
(defstruct trinomial :Polynomial :a :b)
(defmulti xor-mask :Polynomial)

(defn LFSR [t s l] 
  (struct-map lfsr 
    :PRNG :LFSR 
    :polynomial t 
    :state s 
    :length l))

(defn Trinomial [a b] 
  (struct-map trinomial 
    :Polynomial :Trinomial 
    :a a 
    :b b))

(defn xor [a b] (not (= a b)))

(defmethod xor-mask :Trinomial [t]
  (fn [x]
    (xor 
      (xor (bit-test x (:a t))
           (bit-test x (:b t)))
      (bit-test x 0))))

(defmethod clock-rng :LFSR [l]
  (let [s  (:state l)
        p  (:polynomial l)
        b  ((xor-mask p) s)
        sp (bit-shift-right s 1)]
    [(bit-test s 0)
     (assoc l :state 
       (if b
         (bit-set (:length l) sp)
         sp))]))

(def inner 
  (LFSR (Trinomial 31 24) 327 31))

(def outer 
  (LFSR (Trinomial 17 6) 1 17))
