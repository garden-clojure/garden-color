(ns garden.color-test
  (:require [clojure.test :as t :include-macros true]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [garden.color :as c])
  #?(:clj
     (:import garden.color.Hsl
              garden.color.Hsla
              garden.color.Rgb
              garden.color.Rgba)))

(def gen-rgb-channel
  (gen/choose 0 255))

(def gen-hue
  (gen/choose 0 360))

(def gen-saturation
  (gen/choose 0 100))

(def gen-lightness
  (gen/choose 0 100))

(def gen-alpha-channel
  (gen/double* {:infinite? false
                :NaN? false
                :min 0
                :max 1}))

(defspec red-spec
  (prop/for-all [ch gen-rgb-channel]
    (let [rgb (c/Rgb. ch 0 0)
          hsl (c/hsl rgb)]
      (= ch
         (c/red rgb)
         (c/red hsl)))))

(defspec green-spec
  (prop/for-all [ch gen-rgb-channel]
    (let [rgb (c/Rgb. 0 ch 0)
          hsl (c/hsl rgb)]
      (= ch
         (c/green rgb)
         (c/green hsl)))))

(defspec blue-spec
  (prop/for-all [ch gen-rgb-channel]
    (let [rgb (c/Rgb. 0 0 ch)
          hsl (c/hsl rgb)]
      (= ch
         (c/blue rgb)
         (c/blue hsl)))))

(defspec hue-spec
  (prop/for-all [h gen-hue]
    (let [hsl (c/Hsl. h 0 0)]
      (= h (c/hue hsl)))))

(defspec saturation-spec
  (prop/for-all [s gen-saturation]
    (let [hsl (c/Hsl. 0 s 0)]
      (= s
         (c/saturation hsl)))))

(defspec lightness-spec
  (prop/for-all [l gen-lightness]
    (let [hsl (c/Hsl. 0 0 l)
          rgb (c/rgb hsl)]
      (= l
         (c/lightness hsl)))))

(defspec alpha-spec
  (prop/for-all [ch gen-alpha-channel]
    (let [rgba (c/Rgba. 0 0 0 ch)
          hsla (c/Hsla. 0 0 0 ch)]
      (= ch
         (c/alpha rgba)
         (c/alpha hsla)))))

(defspec rgb-spec
  (prop/for-all [r-ch gen-rgb-channel
                 g-ch gen-rgb-channel
                 b-ch gen-rgb-channel]
    (let [rgb (c/Rgb. r-ch g-ch b-ch)]
      (and (= rgb (c/rgb rgb))
           (= r-ch (c/red rgb))
           (= g-ch (c/green rgb))
           (= b-ch (c/blue rgb))))))

(defspec rgba-spec
  (prop/for-all [r-ch gen-rgb-channel
                 g-ch gen-rgb-channel
                 b-ch gen-rgb-channel
                 a-ch gen-alpha-channel]
    (let [rgba (c/Rgba. r-ch g-ch b-ch a-ch)]
      (and (= rgba (c/rgba rgba))
           (= r-ch (c/red rgba))
           (= g-ch (c/green rgba))
           (= b-ch (c/blue rgba))
           (= a-ch (c/alpha rgba))))))

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
