(ns ogl.hull
  (:use [penumbra.opengl])
  (:require [penumbra [text :as text]])
  (:require [penumbra.app :as app])
  (:require [clojure.contrib.monads :as monad])
  (:require [mapatorius.util.hull :as hull])
  (:require [mapatorius.util.random :as rng])
  (:require [mapatorius.util.random.shrink :as shrink]))

(defn init [state]
  (enable :depth-test)
  (enable :multisample)
  (depth-test :lequal)
  (hint :perspective-correction-hint :nicest)
  (hint :polygon-smooth-hint :nicest)
  (app/vsync! true)
  state)

(defn make-point [p]
  (list
    (vertex (+ (:x p) 0.2) (+ (:y p) 0.2) 0)
    (vertex (- (:x p) 0.2) (+ (:y p) 0.2) 0)
    (vertex (- (:x p) 0.2) (- (:y p) 0.2) 0)
    (vertex (+ (:x p) 0.2) (- (:y p) 0.2) 0)))


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

(def some-points
  (map make-point
  (take 100
    (rng/value-seq 
      (sample-hull hull/example-hull) 
      shrink/example-shrink))))

(defn display [[delta time] state]
  (translate 0 0 -20)
  (rotate (:rot-x state) 1 0 0)
  (rotate (:rot-y state) 0 1 0)
  (draw-quads          ;; x y z
    (color 1 0 0)
      (make-square 0 0 0 0.5)
      some-points))

(app/start
  {:display display, :reshape reshape, :mouse-drag mouse-drag, :init init}
  {:rot-x 0, :rot-y 0})
 
