(ns 
  mapatorious.util.hull
  (:refer-clojure :exclude (deftype))
  (:use clojure.contrib.types))

(defstruct point :x :y)
(defn get-x [p] (:x p))
(defn get-y [p] (:y p))

(defstruct edge :a :b)

;; TODO Replace this with linear time median finding
(defn median-by [f ns]
  (let [ns  (sort-by f ns)
        cnt (count ns)
        mid (bit-shift-right cnt 1)]
      (nth ns mid)))

(def example-hull
  (vector
    (struct point 0 1)
    (struct point 1 2)
    (struct point 5 3)
    (struct point 4 1)
    (struct point 3 0)))

(defadt ::bsp-tree
  empty-tree
  (leaf test)
  (node test left-tree right-tree))

(defn bsp-test [t p]
  (match t
    empty-tree false
    (leaf test) (test p)
    (node ts l r) (if (ts p) (bsp-test l p)
                             (bsp-test r p))))

;; Given a hull as a set of points, return the edges
;; Assumes that the hull is sorted clockwise or counter-clockwise   

(defn hull-edges [h]
  (let [sz (count h)
        edges 
          (map (fn [x] (struct edge (nth h x)
                                    (nth h (mod (inc x) sz)))) 
               (range 0 sz))]
    edges))


(defn build-bsp [h dir]
  (let [ac (if dir get-x get-y)]
    (if (> (count h) 1)
      (let [med (median-by ac h)
            pred #(< (ac %) med)]
        (node 
            #(< (ac %) med)
            (build-bsp (filter pred h) (not dir))
            (build-bsp (remove pred h) (not dir))))
        (if (empty h)
            empty-tree
            (leaf (first h))))))
      


(median-by get-x (vector (struct point 3 1) (struct point 2 4)))
(median-by get-y (vector (struct point 3 1) (struct point 2 4)))

(hull-edges example-hull)
