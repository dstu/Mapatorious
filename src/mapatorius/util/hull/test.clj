(ns mapatorius.util.hull.test
  (:require [clojure.contrib.monads :as monad])
  (:require [mapatorius.util.hull :as hull])
  (:require [mapatorius.util.random :as rng])
  (:require [mapatorius.util.random.shrink :as shrink]))

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

(defn start []
  (println (take 10 (rng/value-seq (sample-hull hull/example-hull) shrink/example-shrink))))
