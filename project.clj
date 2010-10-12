(defproject mapatorius "0.0.0"
  :description "A proceudral generation project"
  :dependencies [[penumbra "0.6.0-SNAPSHOT"]
                 [org.clojure/clojure "1.2.0"] 
                 [org.clojure/clojure-contrib "1.2.0"]
                 [clj-kylm "0.0.6"]
                 [bikeshed "0.0.0"]]
  :native-dependencies [[penumbra/lwjgl "2.4.2"]]
  :dev-dependencies [[native-deps "1.0.1"]
         		     [autodoc "0.7.1"]
                     [lein-clojars "0.5.0-SNAPSHOT"]
                     [swank-clojure "1.2.1"]])
