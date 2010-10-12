(ns ogl.sample
  (:use [penumbra.opengl])
  (:require [penumbra [text :as text]])
  (:require [penumbra.app :as app])
  (:require [bikeshed.random :as rng])
  (:require [bikeshed.random.shrink :as shrink]))

(defn init [state]
  (enable :depth-test)
  (enable :multisample)
  (depth-test :lequal)
  (hint :perspective-correction-hint :nicest)
  (hint :polygon-smooth-hint :nicest)
  (app/vsync! true)
  state)

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
 
(defn make-grid []
  (for [x (range 10)
        y (range 10)]
    (make-square x y 0 0.5)))

(defn display [[delta time] state]
  (translate 0 0 -6)
  (rotate (:rot-x state) 1 0 0)
  (rotate (:rot-y state) 0 1 0)
  (draw-quads          ;; x y z
    (color 1 0 0)
      (make-square 0 0 0 0.5))
  (text/with-font 
      (text/font "Inconsolata" :size 50)
      (text/write-to-screen "hello world" 200 200))
      (app/repaint!))


(app/start
  {:display display, :reshape reshape, :mouse-drag mouse-drag, :init init}
  {:rot-x 0, :rot-y 0})
