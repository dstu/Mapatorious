;; A shrinking generator

(defstruct sg :stream :gate)
(defstruct LFSR :trinomial :state :length)
(defstruct trinomial :a :b)

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
  (struct-map LFSR
    :trinomial ((accessor LFSR :trinomial) l)
    :state x))

(defn LSB [l]
  (bit-test 
    ((accessor LFSR :state) l)
    0))

(defn MSB [l]
  (bit-test 
    ((accessor LFSR :state) l)
    ((accessor LFSR :length) l)))

(defn clock [l]
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
  (let [in  ((accessor sg :stream) s)
        out ((accessor sg :gate) s)]
        (struct sg 
                (clock in)
                (clock out))))

(defn clock-n-sg [s n]
  (loop [c n 
         g s]
    (if (zero? c)
        g
        (recur (dec c) (clock-sg g)))))

(defn buffer-sg [s]
  (let [lenA ((accessor LFSR :length) ((accessor sg :stream) s))
        lenB ((accessor LFSR :length) ((accessor sg :gate) s))]
    (clock-n-sg s (max lenA lenB))))

(defn get-bits [s n]
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

(defn get-ints [s n b]
  (loop [c n
         acc (list '() s)]
    (if (<= c 0)
        (first acc)
        (let [accp (get-bits (fnext acc) b)]
          (recur (dec c) (list (cons (first accp) (first acc)) (fnext accp)))))))

(def exsg (struct sg inner outer))
