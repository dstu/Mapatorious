(ns ogl.cloud
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

(def gen shrink/example-shrink)

(defn normal-point []
  (monad/domonad rng/random-m
    [a rng/get-normal
     b rng/get-normal
     c rng/get-normal]
    [a b c]))

(defn point-cloud [g]
  (take 5000
    (rng/value-seq (normal-point) g)))

(def cloud
  (point-cloud gen))

(defn display [[delta time] state]
  (translate -2 0 -6)
  (rotate (:rot-x state) 1 0 0)
  (rotate (:rot-y state) 0 1 0)
  (draw-points          ;; x y z
    (color 1 0 0)
    (doall (map #(apply vertex %) cloud))))

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
 
