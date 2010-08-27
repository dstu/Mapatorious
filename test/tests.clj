(ns test
  (:use [clojure.test])
  (:require
    [ogl.stairs :as stairs]
    [ogl.sample :as sample]
    [ogl.hull   :as hull]
    [ogl.cloud  :as cloud]
    [ogl.lines  :as lines]
    ))

(deftest run
  '(testing "Hull"
    (hull/start))
  '(testing "Cloud"
    (cloud/start))
  '(testing "Lines"
    (lines/start))
  '(testing "Stairs"
    (stairs/start))
    )
