(ns garden.color-test
  (:require
   [clojure.test :refer :all]
   [clojure.test.check :as tc]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [clojure.test.check.clojure-test :refer [defspec]]
   [garden.color :as c]))


(defspec round-trip 10000
  (prop/for-all [i gen/pos-int]
    (= (c/long i)
       (c/long (c/hex i))
       (c/long (c/rgb i))
       (c/long (c/rgba i))
       (c/long (c/hsl i))
       (c/long (c/hsla i)))))

(defspec hsl-to-rgb-is-equal-to-rgb-to-hsl 10000
  (prop/for-all [i gen/pos-int]
    (= (-> i c/hsl c/rgb c/long)
       (-> i c/rgb c/hsl c/long))))

(defspec invert-of-invert-is-equal-to-the-original-value 10000
  (prop/for-all [i gen/pos-int]
    (= (-> i c/invert c/invert c/long)
       i)))
