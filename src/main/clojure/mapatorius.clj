(ns mapatorius
  "Mapatorius main namespace")

(gen-class
 :name Mapatorious)

(defn center-window
  "Centers a Frame or JFrame"
  [frame]
  (let [width (.getWidth frame)
	height (.getHeight frame)
	;; Call a static method. To access a static field, don't
	;; enclose in parens (i.e., don't call it as a function).
	toolkit (java.awt.Toolkit/getDefaultToolkit)
	screenDimension (.getScreenSize (java.awt.Toolkit/getDefaultToolkit))]
    (.setLocation frame
		  (/ (- (.getWidth screenDimension) width) 2)
		  (/ (- (.getHeight screenDimension) height) 2))))
