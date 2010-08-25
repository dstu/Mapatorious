(load-file "shrink.clj") ;; TODO Replace this with a proper require

(defn get-noise-function [sg rng]
  (let [xp (knuth-shuffle sg (vec (range 0 rng)))
        yp (knuth-shuffle (fnext xp) (vec (range 0 rng)))
        zp (knuth-shuffle (fnext yp) (vec (range 0 rng)))
        permX (first xp)
        permY (first yp)
        permZ (first zp)
        gen  (fnext zp)
        nfun (fn [x y z] 
          (bit-xor
            (bit-xor (nth permX x)
                     (nth permY y))
            (nth permZ z)))]
    (list nfun gen)))
