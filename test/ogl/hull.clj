(ns ogl.hull
  (:use [penumbra.opengl])
  (:require [penumbra [text :as text]])
  (:require [penumbra.app :as app])
  (:require [clojure.contrib.monads :as monad])
  (:require [mapatorius.util.hull :as hull])
  (:require [mapatorius.util.random :as rng])
  (:require [mapatorius.util.random.shrink :as shrink]))

(defn make-point [p]
  (do
    (println [(+ (:x p) 0.1) (+ (:y p) 0.1) 0])
    (list
      (vertex (+ (:x p) 0.1) (+ (:y p) 0.1) 0)
      (vertex (- (:x p) 0.1) (+ (:y p) 0.1) 0)
      (vertex (- (:x p) 0.1) (- (:y p) 0.1) 0)
      (vertex (+ (:x p) 0.1) (- (:y p) 0.1) 0))))


(defn reshape [[x y width height] state]
  (frustum-view 60.0 (/ (double width) height) 1.0 100.0)
  (load-identity)
  state)

(defn mouse-drag [[dx dy] [x y] button state]
  (assoc state
    :rot-x (+ (:rot-x state) dy)
    :rot-y (+ (:rot-y state) dx)))

(defn make-square [x y z sz]
  (list
    (vertex (+ x (* 0.7071 sz)) (+ y (* 0.7071 sz)) z)
    (vertex (- x (* 0.7071 sz)) (+ y (* 0.7071 sz)) z)
    (vertex (- x (* 0.7071 sz)) (- y (* 0.7071 sz)) z)
    (vertex (+ x (* 0.7071 sz)) (- y (* 0.7071 sz)) z)))
 

(defn sample-hull [h]
  (monad/domonad rng/random-m
    [gen rng/split-gen]
      (let [[x0 x1 y0 y1] (hull/bounding-box h)]
       (loop [gn gen]
         (let [[a gf] (rng/get-double gn)
               [b gs] (rng/get-double gf)
           p      (hull/Point 
                    (+ (* (- x1 x0) a) x0)
                    (+ (* (- y1 y0) b) y0))]
            (if (hull/inside-hull?
                  (hull/build-bsp (hull/to-angles h) true)
                  p)
                p
                (recur gs)))))))

(defn to-list [lst]
  (loop [l lst
         acc (list)]
    (if (empty? l)
        acc
        (recur (next l)
              (cons (first l) acc)))))

(def some-points
  (let [pt (to-list 
             (take 10
                 (rng/value-seq 
                   (sample-hull hull/example-hull) 
                    shrink/example-shrink)))]
    (do
      (map make-point pt))))

(println (make-point {:x 2.16 :y 2.09}))
(println some-points)

(defn display [[delta time] state]
  (translate 0 0 -20)
  (rotate (:rot-x state) 1 0 0)
  (rotate (:rot-y state) 0 1 0)
  (draw-quads          ;; x y z
    (color 1 0 0)
      (make-square 0 0 0 0.5)
      (list
      (make-point {:x 2.16 :y 2.09})
      (make-point {:x 1.49 :y 1.89}))
      (make-square 1.65 0.92 0 0.1)
      (make-square 4.40 2.02 0 0.1)
      (make-square 3.60 2.48 0 0.1)
      (make-square 3.86 1.84 0 0.1)
      (make-square 1.23 0.79 0 0.1)))


(defn init [state]
  (enable :depth-test)
  (enable :multisample)
  (depth-test :lequal)
  (hint :perspective-correction-hint :nicest)
  (hint :polygon-smooth-hint :nicest)
  (app/vsync! true)
  state)

(app/start
  {:display display, :reshape reshape, :mouse-drag mouse-drag, :init init}
  {:rot-x 0, :rot-y 0})
 
