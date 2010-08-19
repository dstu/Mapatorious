(ns
  clojure.util.select)

(defn median-slow [ns]
  (let [ns  (sort ns)
        cnt (count ns)
        mid (bit-shift-right cnt 1)]
    (nth ns mid)))

(defn trisect [f n]
  (loop [ns n
        [a b c] [[] [] []]]
    (if (empty? ns)
      [a b c]
      (recur
        (next ns)
        (let [x (first ns)]
          (if (> f x)
            [(conj a x) b c]
            (if (< f x)
              [a b (conj c x)]
              [a (conj b x) c])))))))
        
(defn select [l k]
  (if (< (count l) 10)
    (nth (sort l) k)
    (let [meds    (map median-slow (partition 5 l))
          M       (select meds (bit-shift-right (count meds) 1))]
        M)))
;;        (if (<= (count L1))
;;          (select L1 k)
;;          (if (> k csum)
;;            (select L3 (- k csum))
;;            M)))))

(defn median [l]
  (select l (bit-shift-right (count l) 1)))
