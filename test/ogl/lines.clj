(ns ogl.lines
  (:use [penumbra.opengl])
  (:require [penumbra [text :as text]])
  (:require [penumbra.app :as app])
  (:require [clojure.contrib.monads :as monad])
  (:require [bikeshed.hull :as hull])
  (:require [bikeshed.random :as rng])
  (:require [bikeshed.random.shrink :as shrink])
  (:require [bikeshed.random.stop :as stop])
  (:require [bikeshed.random.alternating :as alt]))


(defn reshape [[x y width height] state]
  (frustum-view 60.0 (/ (double width) height) 0.0 15.0)
  (load-identity)
  state)

(defn mouse-drag [[dx dy] [x y] button state]
  (assoc state
    :rot-x (+ (:rot-x state) dy)
    :rot-y (+ (:rot-y state) dx)))

(def gen alt/example-gen)

(defn normal-point []
  (monad/domonad rng/random-m
    [a rng/get-normal
     b rng/get-normal
     c rng/get-normal]
    [a b c]))

(defn point-cloud [g]
  (take 50
    (rng/value-seq (normal-point) g)))

(def cloud
  (point-cloud gen))

(defn vertex-with-color [x y z]
  (do
    (color
      (/ 1 (+ 1 (Math/exp x)))
      (/ 1 (+ 1 (Math/exp y)))
      (/ 1 (+ 1 (Math/exp z))))
    (vertex [x y z])))

(defn display [[delta time] state]
  (rotate (:rot-x state) 1 0 0)
  (rotate (:rot-y state) 0 1 0)
  (draw-line-strip          ;; x y z
    (doall (map #(apply vertex-with-color %) cloud))))
    ;;(doall (map #(apply vertex %) cloud))))

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
 
