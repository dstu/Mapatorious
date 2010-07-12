(ns run
  (:use mapatorius))

(def frame (new javax.swing.JFrame))
(.setTitle frame "Welcome to Mapatorius!")
(.add (.getContentPane frame) (new javax.swing.JButton "Give monies plox"))
(.pack frame)
(.setVisible frame true)
(.addWindowListener frame
		    (proxy [java.awt.WindowAdapter]
			(
			(.dispose frame)))