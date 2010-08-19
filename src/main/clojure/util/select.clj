(ns
  clojure.util.select)

(defn median-slow [f]
  (fn [ns]
    (let [ns  (sort-by f ns)
          cnt (count ns)
          mid (bit-shift-right cnt 1)]
      (nth ns mid))))

(defn bisect [f n]
  (loop [ns n
        [a b] [[] []]]
    (if (empty? ns)
      [a b]
      (recur
        (next ns)
        (let [x (first ns)]
          (if (> f x)
            [(conj a x) b]
            [a (conj b x)]))))))

(defn kth-by [l k f]
  (if (< (count l) 10)
    (nth (sort-by f l) k)
    (let [meds    (map (median-slow f) (partition-all 5 l))
          M       (kth-by meds (bit-shift-right (count meds) 1) f)
          [L1 L2] (bisect M l)]
        (if (< k (count L1))
            (kth-by L1 k f)
            (kth-by L2 (- k (count L1)) f)))))

(defn kth [l k]
  (kth-by l k identity))

(defn median [l]
  (kth-by l (bit-shift-right (count l) 1) identity))

(defn median-by [l f]
  (kth-by l (bit-shift-right (count l) 1) f))
