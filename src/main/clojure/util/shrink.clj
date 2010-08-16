(defstruct sg :stream :gate)
(defstruct LFSR :trinomial :state :length)
(defstruct trinomial :a :b)

(derive ::ShrinkingGenerator ::PRNG)

(defn xor [a b] (not (= a b)))

(defn bang-bit [x n v]
  (if v
    (bit-set x n)
    (bit-clear x n)))

(def inner 
  (struct-map LFSR
    :trinomial (struct-map trinomial
                 :a 31
                 :b 24)
    :state 1
    :length 31))

(def outer
  (struct-map LFSR
    :trinomial (struct-map trinomial
                 :a 17
                 :b 6)
    :state 1
    :length 17))

(defn rekey [l x]
  (assoc l :state x))

(defn rekey-shrink [s sbits gbits]
  (let [gate ((accessor sg :gate) s)
        strm ((accessor sg :stream) s)]
    (assoc
      (assoc s :stream (rekey strm sbits))
      :gate (rekey gate gbits))))

(defn LSB [l]
  (bit-test 
    ((accessor LFSR :state) l)
    0))

(defn MSB [l]
  (bit-test 
    ((accessor LFSR :state) l)
    ((accessor LFSR :length) l)))

(defn clock [l]
  "Advance a linear-feedback shift register"
  (let [s ((accessor LFSR :state) l)
        t ((accessor LFSR :trinomial) l)
        a (bit-test s ((accessor trinomial :a) t))
        b (bit-test s ((accessor trinomial :b) t))
        c (bit-test s 0)
        len ((accessor LFSR :length) l)]
    (struct-map LFSR
      :trinomial t
      :state (bang-bit (bit-shift-right s 1) len (xor (xor a b) c))
      :length len)
    ))

(defn clock-sg [s]
  "Clocks a shrinking generator"
  (let [in  ((accessor sg :stream) s)
        out ((accessor sg :gate) s)]
        (struct sg 
                (clock in)
                (clock out))))

(defn clock-n-sg [s n]
  "Clocks a shrinking generator n times"
  (loop [c n 
         g s]
    (if (zero? c)
        g
        (recur (dec c) (clock-sg g)))))

(defn buffer-sg [s]
  "Buffers a shrinking generator so that the seed does not appear in the output"
  (let [lenA ((accessor LFSR :length) ((accessor sg :stream) s))
        lenB ((accessor LFSR :length) ((accessor sg :gate) s))]
    (clock-n-sg s (max lenA lenB))))

(defn get-bits [s n]
  "Provide n pseudo-random bits. Returns the bits and updated generator"
  (loop [c n
         acc (list 0 s)]
    (if (<= c 0)
        (list (bit-shift-right (first acc) 1) (fnext acc))
        (let [sp (clock-sg (fnext acc))
              gt (LSB ((accessor sg :gate) sp))
              st (LSB ((accessor sg :stream) sp))
              nm (first acc)]
          (if gt
            (recur 
              (dec c) 
              (list (bang-bit (bit-shift-left nm 1) 1 st) sp))
            (recur c (list nm sp)))))))

(defn split-gen [s]
  (let [strm ((accessor LFSR :length) ((accessor sg :stream) s))
        gate ((accessor LFSR :length) ((accessor sg :gate) s))
        n    (get-bits s strm)
        m    (get-bits (second n) gate)
        sbit (first n)
        gbit (first m)
        sg   (second m)]
    (list sg (rekey-shrink s sbit gbit))))

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

;;(defn knuth-shuffle [s v]
;;   (loop [c (dec (count v))
;;          sv v
;;          sg s]
;;     (if (<= c 0)
;;       (list sv sg)
;;       (let [res ((get-int c) sg))
;;             nm  (first res)
;;             ng  (fnext res)
;;             tmpv (nth sv c)
;;             vnext (assoc 
;;                     (assoc sv c (nth sv nm))
;;                     nm tmpv)]
;;         (recur (dec c) vnext ng)))))

(def exsg (buffer-sg (struct sg inner outer)))
