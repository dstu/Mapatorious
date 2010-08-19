(ns 
  mapatorious.util.hull
  (:refer-clojure :exclude (deftype))
  (:use clojure.util.select)
  (:use clojure.contrib.types))

(defstruct rectangle :x0 :x1 :y0 :y1)
(defstruct point :x :y)

(defstruct edge :a :b)
(defstruct angle :point :cw :ccw)


(defn cw? [e q]
  (let [e0  (:a e)
        e1  (:b e)
        e0x (:x e0)
        e0y (:y e0)
        e1x (:x e1)
        e1y (:y e1)
        qx  (:x q)
        qy  (:y q)]
    (>= 0 (- (* (- e1x e0x) 
                (- qy e0y)) 
             (* (- e1y e0y) 
                (- qx e0x))
    ))))

;; TODO Replace this with linear time median finding
(comment (defn median-by [f ns]
  (let [ns  (sort-by f ns)
        cnt (count ns)
        mid (bit-shift-right cnt 1)]
      (nth ns mid))))

(defn bounding-box [h]
  (let [xs (sort (map :x h))
        ys (sort (map :y h))]
    (list  
      (first xs)
      (last  xs)
      (first ys)
      (last  ys))))

(def example-hull
  (vector
    (struct point 0 1)
    (struct point 1 2)
    (struct point 5 3)
    (struct point 4 1)
    (struct point 3 0)))

(defadt ::bsp-tree
  empty-tree
  (leaf hull-edge)
  (node test left-tree right-tree))

(defn bsp-test [t p]
  (match t
    empty-tree false
    (leaf e) (cw? e p)
    (node ts l r)
      (if (ts p) 
        (bsp-test l p)
        (bsp-test r p))))

;; Given a hull as a set of points, return the edges
;; Assumes that the hull is sorted clockwise or counter-clockwise   

(defn hull-edges [h]
  (let [sz (count h)]
    (map 
      (fn [x] 
        (struct edge 
                (nth h x)
                (nth h (mod (inc x) sz))))
      (range 0 sz))))

(defn to-angles [h]
  (let [edges (hull-edges h)
        sz    (count h)
        ccw  #(if (< (dec %) 0) (dec sz) (dec %))]
    (map #(struct angle 
                  (nth h %) 
                  (:b (nth edges %))
                  (:a (nth edges (ccw %))))
         (range 0 sz))))

(defn build-bsp [h dir]
  (let [ac (if dir :x :y)]
    (if (empty? h)
        empty-tree
        (let [med (median-by #(ac (:point %)) h)
              pred #(< (ac (:point %)) (ac (:point med)))
              test #(< (ac %) (ac (:point med)))]
          (if (> (count h) 2)
              (node 
                test
                (build-bsp (filter pred h) (not dir))
                (build-bsp (remove pred h) (not dir)))
              (node
                test
                (leaf (struct edge (:ccw med) (:point med)))
                (leaf (struct edge (:point med) (:cw med))))
            )))))
      
(def exbsp (build-bsp (to-angles example-hull) true))
