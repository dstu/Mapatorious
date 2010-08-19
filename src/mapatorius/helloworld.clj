(ns mapatorius.helloworld
  (:use [penumbra opengl])
  (:require [penumbra.app :as app]
	    [penumbra.text :as text]))

;; Used as callback
(defn init [state]
  "Set up game state once, at beginning of execution."
  (app/title! "Hello, world!"))

;; Used as callback. Has two args, which we ignore.
(defn display [_ _]
  (text/with-font (text/font "Inconsolata" :size 12)
    (text/write-to-screen "Hello, world!" 30 20)
    (app/repaint!)))

;; Call (mapatorius.helloworld/start) to start the display. This
;; calls penumbra.app/start with two arguments specifying
;; certain callbacks and parameters.
(defn start []
  "Start the hello world app."
  (app/start
   {:init init, :display display}
   {}))
