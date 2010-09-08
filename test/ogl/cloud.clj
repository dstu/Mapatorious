(ns ogl.cloud
  (:use [penumbra.opengl])
  (:require [penumbra [text :as text]])
  (:require [penumbra.app :as app])
  (:require [clojure.contrib.monads :as monad])
  (:require [mapatorius.util.hull :as hull])
  (:require [mapatorius.util.random :as rng])
  (:require [mapatorius.util.random.stop :as stop]))


(defn reshape [[x y width height] state]
  (frustum-view 60.0 (/ (double width) height) 0.0 100.0)
  (load-identity)
  state)

(defn mouse-drag [[dx dy] [x y] button state]
  (assoc state
    :rot-x (+ (:rot-x state) dy)
    :rot-y (+ (:rot-y state) dx)))

(def gen1 stop/example-gen)


(defn get-point [f]
  (monad/domonad rng/random-m
    [a f
     b f
     c f]
    [a b c]))

(defn point-cloud [n g f]
  (let [points (take n (rng/value-seq (get-point f) g))]
    (map (fn [[x y z]]
      (let [luminosity (/ 1 (+ 1 (Math/sqrt (+ (* x x) (* y y) (* z z)))))]
       [[(/ luminosity 2)  (/ luminosity 8) luminosity]
       [x y z]])) points)))

(defn vertex-color [cl pt]
  (do
    (color cl)
    (vertex pt)))

(def cloud-normal
  (point-cloud 5000 gen1 rng/get-normal))

(defn display [[delta time] state]
  (translate 0 0 -2)
  (rotate (:rot-x state) 1 0 0)
  (rotate (:rot-y state) 0 1 0)
  (draw-points          ;; x y z
    (color 1 0 0)
    (doall (map #(apply vertex-color %) cloud-normal))))

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
 
