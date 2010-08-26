(ns ogl.hull
  (:use [penumbra.opengl])
  (:require [penumbra [text :as text]])
  (:require [penumbra.app :as app])
  (:require [clojure.contrib.monads :as monad])
  (:require [mapatorius.util.hull :as hull])
  (:require [mapatorius.util.random :as rng])
  (:require [mapatorius.util.random.shrink :as shrink]))


(defn reshape [[x y width height] state]
  (frustum-view 60.0 (/ (double width) height) 1.0 100.0)
  (load-identity)
  state)

(defn mouse-drag [[dx dy] [x y] button state]
  (assoc state
    :rot-x (+ (:rot-x state) dy)
    :rot-y (+ (:rot-y state) dx)))

(def gen (rng/split-gen shrink/example-shrink))
(def gen1 (first gen))
(def gen2 (fnext gen))

(defn make-square [x y z sz]
  (list
    [(+ x (* 0.7071 sz)) (+ y (* 0.7071 sz)) z]
    [(- x (* 0.7071 sz)) (+ y (* 0.7071 sz)) z]
    [(- x (* 0.7071 sz)) (- y (* 0.7071 sz)) z]
    [(+ x (* 0.7071 sz)) (- y (* 0.7071 sz)) z]))
 
(defn make-point [p]
  [(:x p) (:y p) (:z p)])

(defn sample-hull [h]
  (monad/domonad rng/random-m
    [gen rng/split-gen]
      (let [[x0 x1 y0 y1] (hull/bounding-box h)]
       (loop [gn gen]
         (let [[a gf] (rng/get-double gn)
               [b gs] (rng/get-double gf)
               [c gt] (rng/get-normal gs)
           p      {:x (+ (* (- x1 x0) a) x0)
                   :y (+ (* (- y1 y0) b) y0)
                   :z (/ c 5)}]
            (if (hull/inside-hull?
                  (hull/build-bsp (hull/to-angles h) true)
                  p)
                p
                (recur gt)))))))

(defn get-points [h g]
  (map make-point
  (take 1000
    (rng/value-seq 
      (sample-hull h) g))))

(def hull1
  (get-points hull/example-hull gen1))

(def hull2
  (get-points hull/example-hull2 gen2))

(defn display [[delta time] state]
  (translate -2 0 -6)
  (rotate (:rot-x state) 1 0 0)
  (rotate (:rot-y state) 0 1 0)
  (draw-points          ;; x y z
    (color 1 0 0)
    (doall (map #(apply vertex %) hull1))
    (color 0 0 1)
    (doall (map #(apply vertex %) hull2))))

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
 
