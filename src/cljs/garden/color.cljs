(ns garden.color
  (:refer-clojure :exclude [complement long + - * /])
  (:require
   [cljs.core :as clj]
   [clojure.string :as string]
   [clojure.set :as set]))

;; ---------------------------------------------------------------------
;; Color protocols

(defprotocol IRgb
  "Return an instance of Rgb from this."
  (-rgb [this]))

(defprotocol IRgba
  "Return and instance of Rgba from this."
  (-rgba [this]))

(defprotocol IRed
  "Return the red channel value of this."
  (-red [this]))

(defprotocol IGreen
  "Return the green channel value of this."
  (-green [this]))

(defprotocol IBlue
  "Return the blue channel value of this."
  (-blue [this]))

(defprotocol IHsl
  "Return an instance of Hsl from this."
  (-hsl [this]))

(defprotocol IHsla
  "Return an instance of Hsla from this."
  (-hsla [this]))

(defprotocol IHue
  "Return the hue value of this."
  (-hue [this]))

(defprotocol ISaturation
  "Return the saturation value of this."
  (-saturation [this]))

(defprotocol ILightness
  "Return the lightness value of this."
  (-lightness [this]))

(defprotocol ILab
  "Return an instance of (CIELAB) Lab from this."
  (-lab [this]))

(defprotocol ILaba
  "return an instance of (CIELAB) Laba from this."
  (-laba [this]))

(defprotocol IA
  "Return the CIELAB color-opponent dimension 'a' from this"
  (-astar [this]))

(defprotocol IB
  "Return the CIELAB color-opponent dimension 'b' from this"
  (-bstar [this]))

(defprotocol IHcl
  "Return an instance of Hcl from this."
  (-hcl [this]))

(defprotocol IHcla
  "Return an instance of Hcla from this."
  (-hcla [this]))

(defprotocol IChroma
  "return the chroma value from this."
  (-chroma [this]))

(defprotocol IAlpha
  "Return the alpha channel value of this."
  (-alpha [this]))

(defn irgb?
  "true if `x` satisfies `IRgb`, false otherwise."
  [x]
  (satisfies? IRgb x))

(defn irgba?
  "true if `x` satisfies `IRgba`, false otherwise."
  [x]
  (satisfies? IRgba x))

(defn ired?
  "true if `x` satisfies `IRed`, false otherwise."
  [x]
  (satisfies? IRed x))

(defn igreen?
  "true if `x` satisfies `IGreen`, false otherwise."
  [x]
  (satisfies? IGreen x))

(defn iblue?
  "true if `x` satisfies `IBlue`, false otherwise."
  [x]
  (satisfies? IBlue x))

(defn ihsl?
  "true if `x` satisfies `IHsl`, false otherwise."
  [x]
  (satisfies? IHsl x))

(defn ihsla?
  "true if `x` satisfies `IHsla`, false otherwise."
  [x]
  (satisfies? IHsla x))

(defn ihue?
  "true if `x` satisfies `IHue`, false otherwise."
  [x]
  (satisfies? IHue x))

(defn isaturation?
  "true if `x` satisfies `ISaturation`, false otherwise."
  [x]
  (satisfies? ISaturation x))

(defn ilightness?
  "true if `x` satisfies `ILightness`, false otherwise."
  [x]
  (satisfies? ILightness x))

(defn ilab?
  "true if `x` satisfies `ILab`, false otherwise."
  [x]
  (satisfies? ILab x))

(defn ilaba?
  "true if `x` satisfies `ILaba`, false otherwise."
  [x]
  (satisfies? ILaba x))

(defn ia?
  "true if `x` satisfies `IA`, false otherwise."
  [x]
  (satisfies? IA x))

(defn ib?
  "true if `x` satisfies `IB`, false otherwise."
  [x]
  (satisfies? IB x))

(defn ihcl?
  "true if `x` satisfies `IHcl`, false otherwise."
  [x]
  (satisfies? IHcl x))

(defn ihcla?
  "true if `x` satisfies `IHcla`, false otherwise."
  [x]
  (satisfies? IHcla x))

(defn ichroma?
  "true if `x` satisfies `IChroma`, false otherwise."
  [x]
  (satisfies? IChroma x))

(defn ialpha?
  "true if `x` satisfies `IAlpha`, false otherwise."
  [x]
  (satisfies? IAlpha x))

;; ---------------------------------------------------------------------
;; Color types

(defrecord Rgb [r g b])
(defrecord Rgba [r g b a])
(defrecord Hsl [h s l])
(defrecord Hsla [h s l a])
(defrecord Lab [l astar bstar])
(defrecord Laba [l astar bstar a])
(defrecord Hcl [h c l])
(defrecord Hcla [h c l a])

(defn rgb?
  "true if `x` is an instance of `Rgb`, false otherwise."
  [x]
  (instance? Rgb x))

(defn rgba?
  "true if `x` is an instance of `Rgba`, false otherwise."
  [x]
  (instance? Rgba x))

(defn hsl?
  "true if `x` is an instance of `Hsl`, false otherwise."
  [x]
  (instance? Hsl x))

(defn hsla?
  "true if `x` is an instance of `Hsla`, false otherwise."
  [x]
  (instance? Hsla x))

(defn lab?
  "true if `x` is an instance of `Lab`, false otherwise."
  [x]
  (instance? Lab x))

(defn laba?
  "true if `x` is an instance of `Laba`, false otherwise."
  [x]
  (instance? Laba x))

(defn hcl?
  "true if `x` is an instance of `Hcl`, false otherwise."
  [x]
  (instance? Hcl x))

(defn hcla?
  "true if `x` is an instance of `Hcla`, false otherwise."
  [x]
  (instance? Hcla x))

;; ---------------------------------------------------------------------
;; Color functions


;;; Rgb functions

(defn ^Number red
  "Return an integer (between 0 and 255) of the red channel of x. x must
  satisfy IRed."
  [x]
  {:post [(number? %) (<= 0 % 255)]}
  (-red x))

(defn ^Number green
  "Return an integer (between 0 and 255) of the green channel of x. x 
  must satisfy IGreen."
  [x]
  {:post [(number? %) (<= 0 % 255)]}
  (-green x))

(defn ^Number blue
  "Return an integer (between 0 and 255) of the blue channel of x. x must
  satisfy IBlue."
  [x]
  {:post [(number? %) (<= 0 % 255)]}
  (-blue x))


;;; Hsl, Lab, and Hcl functions

(defn ^Number hue
  [x]
  {:post [(number? %)]}
  (-hue x))

(defn ^Number saturation
  [x]
  {:post [(number? %) (<= 0 % 100)]}
  (-saturation x))

(defn ^Number lightness
  [x]
  {:post [(number? %) (<= 0 % 100)]}
  (-lightness x))

(defn ^Number astar
  [x]
  {:post [(number? %)]}
  ;; TODO: include the proper domain. Read all the papers.
  (-astar x))

(defn ^Number bstar
  [x]
  {:post [(number? %)]}
  ;; TODO: include the proper domain. Read all the papers.
  (-bstar x))

(defn ^Number chroma
  [x]
  {:post [(number? %)]}
  (-chroma x))

(defn ^Number alpha
  [x]
  {:post [(number? %) (<= 0 % 1)]}
  (-alpha x))

;; ---------------------------------------------------------------------
;; Creation

(defn ^Rgb rgb
  "Return an instance of Rgb. x must satisfy either IRgb or all of
  IRed, IGreen, and IBlue."
  [x]
  {:post [(instance? Rgb %)]}
  (if (satisfies? IRgb x)
    (-rgb x)
    (Rgb. (red x) (green x) (blue x))))

(defn ^Rgba rgba
  "Return an instance of Rgba. x must satisfy either IRgba or all of
  IRed, IGreen, IBlue, and IAlpha."
  [x]
  {:post [(instance? Rgba %)]}
  (if (satisfies? IRgba x)
    (-rgba x)
    (Rgba. (red x) (green x) (blue x) (lightness x))))

(defn ^Hsl hsl
  "Return an instance of Hsl. x must satisfy either IHsl or all of
  IHue, ISaturation, and ILightness."
  [x]
  {:post [(instance? Hsl %)]}
  (if (satisfies? IHsl x)
    (-hsl x)
    (Hsl. (hue x) (saturation x) (lightness x))))

(defn ^Hsl hsla
  "Return an instance of Hsla. x must satisfy either IHsla or all of
  IHue, ISaturation, ILightness, and IAlpha."
  [x]
  {:post [(instance? Hsla %)]}
  (if (satisfies? IHsla x)
    (-hsla x)
    (Hsla. (hue x) (saturation x) (lightness x) (alpha x))))

(defn ^Lab lab
  "Return an instance of Lab. x must satisfy either ILab or all of
  ILightness, IA, and IB."
  [x]
  {:post [(instance? Lab %)]}
  (if (satisfies? ILab x)
    (-lab x)
    (Lab. (lightness x) (astar x) (bstar x))))

(defn ^Laba laba
  "Return an instance of Laba. x must satisfy either ILaba or all of
  ILightness, IA, IB, and IAlpha"
  [x]
  {:post [(instance? Laba %)]}
  (if (satisfies? ILaba x)
    (-laba x)
    (Laba. (lightness x) (astar x) (bstar x) (alpha x))))

(defn ^Hcl hcl
  "Return an instance of Hcl. x must satisfy either IHcl or all of
  IHue, IChroma, and ILightness."
  [x]
  {:post [(instance? Hcl %)]}
  (if (satisfies? IHcl x)
    (-hcl x)
    (Hcl. (hue x) (chroma x) (lightness x))))

(defn ^Hcla hcla
  "Return an instance of Hcla. x must satisfy either IHcla or all of
  IHue, IChroma, ILightness, and IAlpha."
  [x]
  {:post [(instance? Hcla %)]}
  (if (satisfies? IHcla x)
    (-hcla x)
    (Hcla. (hue x) (chroma x) (lightness x) (alpha x))))

(defn ^String hex
  "Convert a color to string in hex format."
  [x]
  (let [c (rgb x)
        f (fn [i]
            (let [s (.toString i 16)]
              (if (= (.-length ^String s) 1)
                (str 0 s)
                s)))
        r (f (.-r c))
        g (f (.-g c))
        b (f (.-b c))]
    (str "#" r g b)))

(defn ^Number long
  "Convert a color to number. x must satisfy IRgb."
  [x]
  (if (number? x)
    x
    (let [c (rgb x)
          r (.-r c)
          g (.-g c)
          b (.-b c)]
      (bit-or (bit-shift-left r 16)
              (bit-shift-left g 8)
              b))))

;; ---------------------------------------------------------------------
;; Conversion

(defn ^Hsl rgb->hsl
  "Return an instance of Hsl from red, green, and blue values."
  [r g b]
  (let [+ clj/+
        - clj/-
        * clj/*
        / clj//
        mn (min r g b)
        mx (max r g b)
        h (cond
            (= mn mx)
            0

            (= r mx)
            (* 60 (/ (- g b)
                     (- mx mn)))

            (= g mx)
            (* 60 (+ 2 (/ (- b r)
                          (- mx mn))))

            :else
            (* 60 (+ 4 (/ (- r g)
                          (- mx mn)))))
        l (/ (+ mn mx) 510)
        s (cond
            (= mn mx)
            0

            (< l 0.5)
            (/ (- mx mn)
               (+ mx mn))

            :else
            (/ (- mx mn)
               (- 510 mx mn)))]
    (Hsl. (mod (float h) 360) (* 100.0 s) (* 100.0 l))))

(defn ^Rgb hsl->rgb
  "Return an instance of Rgb from hue, saturation, and lightness values."
  [h s l]
  (let [+ clj/+
        - clj/-
        * clj/*
        / clj//
        h (mod h 360)
        s (/ s 100.0)
        l (/ l 100.0)
        ;; C = (1 - |2L - 1|) * s
        c (* (- 1 (Math/abs (- (* 2 l) 1))) s)
        ;; X = C * (1 - |(H/60) mod 2 - 1|)
        x (* c (- 1 (Math/abs (- 1 (mod (/ h 60.0) 2)))))
        ;; m = L - C/2
        m (- l (/ c 2.0))
        [r' g' b']
        (cond
         (or (= h 0)   (< 0 h 60))    [c x 0]
         (or (= h 60)  (< 60 h 120))  [x c 0]
         (or (= h 120) (< 120 h 180)) [0 c x]
         (or (= h 180) (< 180 h 240)) [0 x c]
         (or (= h 240) (< 240 h 300)) [x 0 c]
         (or (= h 300) (< 300 h 360)) [c 0 x])
        r (Math/round (* 0xff (+ r' m)))
        g (Math/round (* 0xff (+ g' m)))
        b (Math/round (* 0xff (+ b' m)))]
    (Rgb. r g b)))

(def ^:private lab-constants
  " Note: xyz-lab will be called f(t) below for brevity and consistency with literature.

  xn, yn, zn - CIE XYZ tristimulus values for the white point under Illuminant D65.
  (See https://en.wikipedia.org/wiki/Lab_color_space#CIELAB-CIEXYZ_conversions)

  t3 - The point at which f(t) is made discontinuous.
  Because f(t) approaches infinity at t=0, f(t) is made linear below below this value.
  An approximation of (6/29^3).

  t2 - The slope of the linear part of f(t).
  An approximation of the inverse of (1/3)((29/6)^2).

  t1 - The slope of the reverse transformation of f(t).
  An approximation of 3(6/29)^2.

  t0 - The intercept of the linear part of f(t).
  An approximation of 4/29."
  {:xn 0.950470
   :yn 1
   :zn 1.088830
   :t0 0.137931034
   :t1 0.206896552
   :t2 0.12841855
   :t3 0.008856452})

(defn- rgb->xyz
  "Intermediate conversion from rgb to CIEXYZ"
  [r g b]
  (let [/ clj//
        + clj/+
        * clj/*
        - clj/-
        rgb-xyz (fn [r]
                  (let [r (/ r 255)]
                    (if (<= r 0.04045)
                      (/ r 12.92)
                      (js/Math.pow (/ (+ r 0.055) 1.055) 2.4))))
        xyz-lab (fn [t]
                  (let [{:keys [t0 t2 t3]} lab-constants]
                    (if (> t t3)
                      (js/Math.pow t (/ 1 3))
                      (+ t0 (/ t t2)))))
        rgb (mapv rgb-xyz [r g b])
        {:keys [xn yn zn]} lab-constants]
    (mapv
      (fn [consts n]
        (xyz-lab (/ (apply + (map * consts rgb)) n)))
      [[0.4124564 0.3575761 0.1804375]
       [0.2126729 0.7151522 0.0721750]
       [0.0193339 0.1191920 0.9503041]]
      [xn yn zn])))

(defn ^Lab rgb->lab
  "Return an instance of Lab from red, green, and blue values."
  [r g b]
  (let [- clj/-
        * clj/*
        + clj/*
        / clj//
        [x y z] (rgb->xyz r g b)]
    (Lab.
      (- (* 116 y) 16)
      (* 500 (- x y))
      (* 200 (- y z)))))

(defn ^Rgb lab->rgb
  "Return an instance of Rgb from lightness, a, and b values."
  [l a b]
  (let [- clj/-
        * clj/*
        + clj/*
        / clj//
        {:keys [t0 t1 t2 xn yn zn]} lab-constants
        limit (comp (partial min 255)
                (partial max 0))
        lab-xyz (fn [t]
                  (if (> t t1)
                    (* t t t)
                    (* t2 (- t t0))))
        xyz-rgb (fn [r]
                  (js/Math.round
                    (* 255 (if (<= r 0.00304)
                             (* 12.92 r)
                             (- (* 1.055 (js/Math.pow r
                                           (/ 1 2.4)))
                               0.055)))))
        y (* yn (lab-xyz (/ (+ 16 l) 116)))
        x (* xn (lab-xyz (if (nil? a) y (+ y (/ a 500)))))
        z (* zn (lab-xyz (if (nil? b) y (- y (/ b 200)))))
        [r g b] (mapv
                  (fn [consts]
                    (limit (apply + (map * consts [x y z]))))
                  [[3.2404542 -1.5371385 -0.4985314]
                   [-0.9692660 1.8760108 0.0415560]
                   [0.0556434 -0.2040259 1.0572252]])]
    (Rgb. r g b)))

(defn ^Hcl lab->hcl
  "Return an instance of Hcl from lightness, a, and b values."
  [l a b]
  (let [- clj/-
        * clj/*
        + clj/*
        / clj//
        chroma (js/Math.sqrt (+ (* a a) (* b b)))
        hue (when (not (zero? (js/Math.round (* chroma 10000))))
              (rem (+ 360
                     (* (js/Math.atan2 b a)
                       (/ 180 js/Math.PI)))
                360))]
    (Hcl. hue chroma l)))

(defn ^Lab hcl->lab
  "Return an instance of lab from hue, chroma, and lightness values."
  [h c l]
  (let [- clj/-
        * clj/*
        + clj/*
        / clj//
        radh (* h (/ js/Math.PI 180))]
    (Lab. l
      (* c (js/Math.cos radh))
      (* c (js/Math.sin radh)))))

(defn ^Hcl rgb->hcl
  "Return an instance of Hcl from red, green, and blue values."
  [r g b]
  (let [{:keys [l opp-a opp-b]} (rgb->lab r g b)]
    (lab->hcl l opp-a opp-b)))

(defn ^Rgb hcl->rgb
  "Return an instance of Rgb from hue, chroma, and lightness values."
  [h c l]
  (let [{:keys [l opp-a opp-b]} (hcl->lab h c l)]
    (lab->rgb l opp-a opp-b)))

(defn ^Hsl lab->hsl
  "Return an instance of Hsl from lightness, a, and b values."
  [l a b]
  (let [{:keys [r g b]} (lab->rgb l a b)]
    (rgb->hsl r g b)))

(defn ^Lab hsl->lab
  "Return an instance of Lab from hue, saturation, and lightness values."
  [h s l]
  (let [{:keys [r g b]} (hsl->rgb h s l)]
    (rgb->lab r g b)))

(defn ^Hsl hcl->hsl
  "Return an instance of Hsl from hue, chroma, and lightness values."
  [h c l]
  (let [{:keys [r g b]} (hcl->rgb h c l)]
    (rgb->hsl r g b)))

(defn ^Hcl hsl->hcl
  "Return an instance of Hcl from hue, saturation, and lightness values."
  [h s l]
  (let [{:keys [r g b]} (hsl->rgb h s l)]
    (rgb->hcl r g b)))

;; ---------------------------------------------------------------------
;; Arithmetic

(defn ^Rgb +
  "Compute the sum of the Rgb channels of one or more colors."
  ([]
     ;; Black is the zero of Rgb.
     (Rgb. 0 0 0))
  ([a]
     (rgb a))
  ([a b]
     (merge-with
      (fn [a b]
        (min 255 (clj/+ a b)))
      (rgb a)
      (rgb b)))
  ([a b & more]
     (reduce + (+ a b) more)))

(defn ^Rgb -
  "Compute the difference of the Rgb channels of one or more colors."
  ([a]
     ;; Since negative Rgb values don't exist I'm not sure what the
     ;; right thing to do is. Would inversion make sense?
     (rgb a))
  ([a b]
     (let [c1 (rgb a) 
           c2 (rgb b)]
       (merge-with
        (fn [a b]
          (max 0 (clj/- a b)))
        c1
        c2)))
  ([a b & more]
     (reduce - (- a b) more)))

(defn ^Rgb *
  "Compute the product of the Rgb channels of one or more colors."
  ([]
     ;; White is the one of Rgb.
     (Rgb. 255 255 255))
  ([a]
     (rgb a))
  ([a b]
     (merge-with
      (fn [a b]
        (clj// (clj/* a b)
               255.0))
      (rgb a)
      (rgb b)))
  ([a b & more]
     (reduce * (* a b) more)))

(defn ^Rgb /
  "Compute the quotient of the Rgb channels of one or more colors."
  ([a]
     (merge-with clj//
                 (rgb a)
                 (Rgb. 255.0 255.0 255.0)))
  ([a b]
     (merge-with clj// 
                 (rgb a)
                 (rgb b)))
  ([a b & more]
     (reduce / (/ a b) more)))


;; ---------------------------------------------------------------------
;; Utilities

(defn ^Hsla rotate-hue
  "Rotates the hue value of a given color by degrees."
  [color degrees]
  (update-in (hsla color) [:h] (fn [h] (mod (clj/+ h degrees) 360))))

(defn ^Hsla saturate
  "Increase the saturation value of a given color by amount."
  [color amount]
  (update-in (hsla color) [:s] (fn [s] (min (clj/+ s amount) 100))))

(defn ^Hsla desaturate
  "Decrease the saturation value of a given color by amount."
  [color amount]
  (update-in (hsla color) [:s] (fn [s] (max 0 (clj/- s amount)))))

(defn ^Hsla lighten
  "Increase the lightness value a given color by amount."
  [color amount]
  (update-in (hsla color) [:l] (fn [l] (min (clj/+ l amount) 100))))

(defn ^Hsla darken
  "Decrease the saturation value of a given color by amount."
  [color amount]
  (update-in (hsla color) [:l] (fn [l] (max 0 (clj/- l amount)))))

(defn ^Rgba invert
  "Return the inversion of a color."
  [color]
  (let [c (rgba color)]
    (Rgba. (clj/- 255 (.-r c))
           (clj/- 255 (.-g c))
           (clj/- 255 (.-b c))
           (.-a c))))

(defn ^Rgba mix
  "Mix two or more colors by averaging their Rgba channels."
  ([color-1 color-2]
     (let [c1 (rgba color-1)
           c2 (rgba color-2)]
       (merge-with
        (fn [l r]
          (clj// (clj/+ l r) 2.0))
        c1 c2)))
  ([color-1 color-2 & more]
     (reduce mix (mix color-1 color-2) more)))

(defn hue-rotations
  [color & amounts]
  (map (fn [amount]
         (rotate-hue color amount))
       amounts))

(defn complement
  "Return the complement of a color."
  [color]
  (rotate-hue color 180))

(defn split-complement
  "Given a color return a triple of the color and the two colors on
  either side of it's complement."
  ([color]
     (split-complement color 130))
  ([color distance-from-complement]
     (let [d (max 1 (min 179 distance-from-complement))]
       (hue-rotations color 0 d (clj/- d)))))

(defn analogous
  "Given a color return a triple of colors which are 0, 30, and 60
  degrees clockwise from it. If a second falsy argument is passed the
  returned values will be in a counter-clockwise direction."
  ([color]
     (analogous color true))
  ([color clockwise?]
     (let [sign (if clockwise? clj/+ clj/-)]
       (hue-rotations color 0 (sign 30) (sign 60)))))

(defn triad
  "Given a color return a triple of colors which are equidistance apart
  on the color wheel."
  [color]
  (hue-rotations color 0 120 240))

(defn tetrad
  "Given a color return a quadruple of four colors which are
  equidistance on the color wheel (ie. a pair of complements). An
  optional angle may be given for color of the second complement in the
  pair (this defaults to 90 when only color is passed)."
  ([color]
     (tetrad color 90))
  ([color angle]
     (let [degrees (max 1 (min 90 (Math/abs angle)))
           color' (rotate-hue color degrees)]
       [(rotate-hue color 0)
        (complement color)
        color'
        (complement color')])))

(defn shades
  "Given a color return a list of shades from lightest to darkest by
  a step. By default the step is 10. White and black are excluded from
  the returned list."
  ([color]
     (shades color 10))
  ([color step]
     (let [c (hsl color)]
       (for [i (range 1 (Math/floor (clj// 100.0 step)))]
         (assoc c :l (clj/* i step))))))


;; ---------------------------------------------------------------------
;; Implementation


;;; Color types

(extend-type Rgb
   IRgb
   (-rgb [this] this)

   IRgba
   (-rgba [this]
     (Rgba. (.-r this) (.-g this) (.-b this) 1.0))

   IHsl
   (-hsl [this]
     (rgb->hsl (.-r this) (.-g this) (.-b this)))

   IHsla
   (-hsla [this]
     (let [hsl ^Hsl (-hsl this)]
       (Hsla. (.-h hsl) (.-s hsl) (.-l hsl) 1.0)))

   ILab
   (-lab [this]
     (rgb->lab (.-r this) (.-g this) (.-b this)))

   ILaba
   (-laba [this]
     (let [lab ^Lab (-lab this)]
       (Laba. (.-l lab) (.-astar lab) (.-bstar lab) 1.0)))

   IHcl
   (-hcl [this]
     (rgb->hcl (.-r this) (.-g this) (.-b this)))

   IHcla
   (-hcla [this]
     (let [hcl ^Hcl (-hcl this)]
       (Hcla. (.-h hcl) (.-c hcl) (.-l hcl) 1.0)))

   IRed
   (-red [this]
     (.-r this))

   IGreen
   (-green [this]
     (.-g this))

   IBlue
   (-blue [this]
     (.-b this))

   IAlpha
   (-alpha [_] 1.0)

   IHue
   (-hue [this]
     (.-h ^Hsl (-hsl this)))

   ISaturation
   (-saturation [this]
     (.-s ^Hsl (-hsl this)))

   ILightness
   (-lightness [this]
     (.-l ^Hsl (-hsl this)))

   IA
   (-astar [this]
     (.-astar ^Lab (-lab this)))

   IB
   (-bstar [this]
     (.-bstar ^Lab (-lab this)))

   IChroma
   (-chroma [this]
     (.-c ^Hcl (-hcl this))))

(extend-type Rgba
  IRgb
  (-rgb [this]
    (Rgb. (.-r this) (.-g this) (.-b this)))

  IRgba
  (-rgba [this] this)

  IHsl
  (-hsl [this]
    (rgb->hsl (.-r this) (.-g this) (.-b this)))

  IHsla
  (-hsla [this]
    (let [hsl ^Hsl (-hsl this)]
      (Hsla. (.-h hsl) (.-s hsl) (.-l hsl) (.-a this))))

  ILab
  (-lab [this]
    (rgb->lab (.-r this) (.-g this) (.-b this)))

  ILaba
  (-laba [this]
    (let [lab ^Lab (-lab this)]
      (Laba. (.-l lab) (.-astar lab) (.-bstar lab) (.-a this))))

  IHcl
  (-hcl [this]
    (rgb->hcl (.-r this) (.-g this) (.-b this)))

  IHcla
  (-hcla [this]
    (let [hcl ^Hcl (-hcl this)]
      (Hcla. (.-h hcl) (.-c hcl) (.-l hcl) (.-a this))))

  IAlpha
  (-alpha [this]
    (.-a this))

  IRed
  (-red [this]
    (.-r this))

  IGreen
  (-green [this]
    (.-g this))

  IBlue
  (-blue [this]
    (.-b this))

  IHue
  (-hue [this]
    (.-h ^Hsl (-hsl this)))

  ISaturation
  (-saturation [this]
    (.-s ^Hsl (-hsl this)))

  ILightness
  (-lightness [this]
    (.-l ^Hsl (-hsl this)))

  IA
  (-astar [this]
    (.-astar ^Lab (-lab this)))

  IB
  (-bstar [this]
    (.-bstar ^Lab (-lab this)))

  IChroma
  (-chroma [this]
    (.-c ^Hcl (-hcl this))))

(extend-type Hsl
  IHsl
  (-hsl [this] this)

  IHsla
  (-hsla [this]
    (Hsla. (.-h this) (.-s this) (.-l this) 1.0))

  IRgb
  (-rgb [this]
    (hsl->rgb (.-h this) (.-s this) (.-l this)))

  IRgba
  (-rgba [this]
    (let [rgb (hsl->rgb (.-h this) (.-s this) (.-l this))]
      (Rgba. (.-r rgb) (.-g rgb) (.-b rgb) 1.0)))

  ILab
  (-lab [this]
    (hsl->lab (.-h this) (.-s this) (.-l this)))

  ILaba
  (-laba [this]
    (let [lab ^Lab (-lab this)]
      (Laba. (.-l lab) (.-astar lab) (.-bstar lab) 1.0)))

  IHcl
  (-hcl [this]
    (hsl->hcl (.-h this) (.-s this) (.-l this)))

  IHcla
  (-hcla [this]
    (let [hcl ^Hcl (-hcl this)]
      (Hcla. (.-h hcl) (.-c hcl) (.-l hcl) 1.0)))

  IAlpha
  (-alpha [_] 1.0)

  IRed
  (-red [this]
    (.-r (rgb this)))

  IGreen
  (-green [this]
    (.-g (rgb this)))

  IBlue
  (-blue [this]
    (.-b (rgb this)))

  IHue
  (-hue [this]
    (.-h this))

  ISaturation
  (-saturation [this]
    (.-s this))

  ILightness
  (-lightness [this]
    (.-l this))

  IA
  (-astar [this]
    (.-astar ^Lab (-lab this)))

  IB
  (-bstar [this]
    (.-bstar ^Lab (-lab this)))

  IChroma
  (-chroma [this]
    (.-c ^Hcl (-hcl this))))

(extend-type Hsla
  IHsl
  (-hsl [this]
    (Hsl. (.-h this) (.-s this) (.-l this)))

  IHsla
  (-hsla [this] this)

  IRgb
  (-rgb [this]
    (hsl->rgb (.-h this) (.-s this) (.-l this)))

  IRgba
  (-rgba [this]
    (let [rgb (hsl->rgb (.-h this) (.-s this) (.-l this))]
      (Rgba. (.-r rgb) (.-g rgb) (.-b rgb) (.-a this))))

  ILab
  (-lab [this]
    (hsl->lab (.-h this) (.-s this) (.-l this)))

  ILaba
  (-laba [this]
    (let [lab ^Lab (-lab this)]
      (Laba. (.-l lab) (.-astar lab) (.-bstar lab) (.-a this))))

  IHcl
  (-hcl [this]
    (hsl->hcl (.-h this) (.-s this) (.-l this)))

  IHcla
  (-hcla [this]
    (let [hcl ^Hcl (-hcl this)]
      (Hcla. (.-h hcl) (.-c hcl) (.-l hcl) (.-a this))))

  IAlpha
  (-alpha [this]
    (.-a this))

  IRed
  (-red [this]
    (.-r (rgb this)))

  IGreen
  (-green [this]
    (.-g (rgb this)))

  IBlue
  (-blue [this]
    (.-b (rgb this)))

  IHue
  (-hue [this]
    (.-h this))

  ISaturation
  (-saturation [this]
    (.-s this))

  ILightness
  (-lightness [this]
    (.-l this))

  IA
  (-astar [this]
    (.-astar ^Lab (-lab this)))

  IB
  (-bstar [this]
    (.-bstar ^Lab (-lab this)))

  IChroma
  (-chroma [this]
    (.-c ^Hcl (-hcl this))))

(extend-type Lab
  ILab
  (-lab [this] this)

  ILaba
  (-laba [this]
    (Laba. (.-l this) (.-astar this) (.-bstar this) 1.0))

  IHsl
  (-hsl [this]
    (lab->hsl (.-l this) (.-astar this) (.-bstar this)))

  IHsla
  (-hsla [this]
    (let [hsl ^Hsl (-hsl this)]
      (Hsla. (.-h hsl) (.-s hsl) (.-l hsl) 1.0)))

  IRgb
  (-rgb [this]
    (lab->rgb (.-l this) (.-astar this) (.-bstar this)))

  IRgba
  (-rgba [this]
    (let [rgb ^Rgb (-rgb this)]
      (Rgba. (.-r rgb) (.-g rgb) (.-b rgb) 1.0)))

  IHcl
  (-hcl [this]
    (lab->hcl (.-l this) (.-astar this) (.-bstar this)))

  IHcla
  (-hcla [this]
    (let [hcl ^Hcl (-hcl this)]
      (Hcla. (.-h hcl) (.-c hcl) (.-l hcl) 1.0)))

  IAlpha
  (-alpha [this] 1.0)

  IRed
  (-red [this]
    (.-r (rgb this)))

  IGreen
  (-green [this]
    (.-g (rgb this)))

  IBlue
  (-blue [this]
    (.-b (rgb this)))

  IHue
  (-hue [this]
    (.-h ^Hsl (-hsl this)))

  ISaturation
  (-saturation [this]
    (.-s ^Hsl (-hsl this)))

  ILightness
  (-lightness [this]
    (.-l this))

  IA
  (-astar [this]
    (.-astar this))

  IB
  (-bstar [this]
    (.-bstar this))

  IChroma
  (-chroma [this]
    (.-c ^Hcl (-hcl this))))

(extend-type Laba
  ILab
  (-lab [this]
    (Lab. (.-l this) (.-astar this) (.-bstar this)))

  ILaba
  (-laba [this] this)

  IHsl
  (-hsl [this]
    (lab->hsl (.-l this) (.-astar this) (.-bstar this)))

  IHsla
  (-hsla [this]
    (let [hsl ^Hsl (-hsl this)]
      (Hsla. (.-h hsl) (.-s hsl) (.-l hsl) (.-a this))))

  IRgb
  (-rgb [this]
    (lab->rgb (.-l this) (.-astar this) (.-bstar this)))

  IRgba
  (-rgba [this]
    (let [rgb ^Rgb (-rgb this)]
      (Rgba. (.-r rgb) (.-g rgb) (.-b rgb) (.-a this))))

  IHcl
  (-hcl [this]
    (lab->hcl (.-l this) (.-astar this) (.-bstar this)))

  IHcla
  (-hcla [this]
    (let [hcl ^Hcl (-hcl this)]
      (Hcla. (.-h hcl) (.-c hcl) (.-l hcl) (.-a this))))

  IAlpha
  (-alpha [this]
    (.-a this))

  IRed
  (-red [this]
    (.-r (rgb this)))

  IGreen
  (-green [this]
    (.-g (rgb this)))

  IBlue
  (-blue [this]
    (.-b (rgb this)))

  IHue
  (-hue [this]
    (.-h ^Hsl (-hsl this)))

  ISaturation
  (-saturation [this]
    (.-s ^Hsl (-hsl this)))

  ILightness
  (-lightness [this]
    (.-l this))

  IA
  (-astar [this]
    (.-astar this))

  IB
  (-bstar [this]
    (.-bstar this))

  IChroma
  (-chroma [this]
    (.-c ^Hcl (-hcl this))))

(extend-type Hcl
  IHcl
  (-hcl [this] this)

  IHcla
  (-hcla [this]
    (Hcla. (.-h hcl) (.-c hcl) (.-l hcl) 1.0))

  ILab
  (-lab [this]
    (hcl->lab (.-h hcl) (.-c hcl) (.-l hcl)))

  ILaba
  (-laba [this]
    (let [lab ^Lab (-lab this)]
      (Laba. (.-l lab) (.-astar lab) (.-bstar lab) 1.0)))

  IHsl
  (-hsl [this]
    (hcl->hsl (.-h this) (.-c this) (.-l this)))

  IHsla
  (-hsla [this]
    (let [hsl ^Hsl (-hsl this)]
      (Hsla. (.-h hsl) (.-s hsl) (.-l hsl) 1.0)))

  IRgb
  (-rgb [this]
    (hcl->rgb (.-h this) (.-c this) (.-l this)))

  IRgba
  (-rgba [this]
    (let [rgb ^Rgb (-rgb this)]
      (Rgba. (.-r rgb) (.-g rgb) (.-b rgb) 1.0)))

  IAlpha
  (-alpha [this] 1.0)

  IRed
  (-red [this]
    (.-r (rgb this)))

  IGreen
  (-green [this]
    (.-g (rgb this)))

  IBlue
  (-blue [this]
    (.-b (rgb this)))

  IHue
  (-hue [this]
    (.-h this))

  ISaturation
  (-saturation [this]
    (.-s ^Hsl (-hsl this)))

  ILightness
  (-lightness [this]
    (.-l this))

  IA
  (-astar [this]
    (.-astar ^Lab (-lab this)))

  IB
  (-bstar [this]
    (.-bstar ^Lab (-lab this)))

  IChroma
  (-chroma [this]
    (.-c this)))

(extend-type Hcla
  IHcl
  (-hcl [this]
    (Hcla. (.-h this) (.-c this) (.-l this) 1))

  IHcla
  (-hcla [this] this)

  ILab
  (-lab [this]
    (hcl->lab (.-h hcl) (.-c hcl) (.-l hcl)))

  ILaba
  (-laba [this]
    (let [lab ^Lab (-lab this)]
      (Laba. (.-l lab) (.-astar lab) (.-bstar lab) (.-a this))))

  IHsl
  (-hsl [this]
    (hcl->hsl (.-h this) (.-c this) (.-l this)))

  IHsla
  (-hsla [this]
    (let [hsl ^Hsl (-hsl this)]
      (Hsla. (.-h hsl) (.-s hsl) (.-l hsl) (.-a this))))

  IRgb
  (-rgb [this]
    (hcl->rgb (.-h this) (.-c this) (.-l this)))

  IRgba
  (-rgba [this]
    (let [rgb ^Rgb (-rgb this)]
      (Rgba. (.-r rgb) (.-g rgb) (.-b rgb) (.-a this))))

  IAlpha
  (-alpha [this]
    (.-a this))

  IRed
  (-red [this]
    (.-r (rgb this)))

  IGreen
  (-green [this]
    (.-g (rgb this)))

  IBlue
  (-blue [this]
    (.-b (rgb this)))

  IHue
  (-hue [this]
    (.-h this))

  ISaturation
  (-saturation [this]
    (.-s ^Hsl (-hsl this)))

  ILightness
  (-lightness [this]
    (.-l this))

  IA
  (-astar [this]
    (.-astar ^Lab (-lab this)))

  IB
  (-bstar [this]
    (.-bstar ^Lab (-lab this)))

  IChroma
  (-chroma [this]
    (.-c this)))

;;; Numbers

(extend-type number
  IRgb
  (-rgb [l]
    (Rgb. (-red l) (-green l) (-blue l)))

  IRgba
  (-rgba [l]
    (Rgba. (-red l) (-green l) (-blue l) (-alpha l)))

  IHsl
  (-hsl [l]
    (hsl (-rgb l)))

  IAlpha
  (-alpha [l]
    (if (< 0 (bit-and l 0xff000000))
      (/ (bit-and (bit-shift-right l 24) 0xff)
         255.0)
      1.0))

  IRed
  (-red [l]
    (bit-and (bit-shift-right l 16) 0xff))

  IGreen
  (-green [l]
    (bit-and (bit-shift-right l 8) 0xff))

  IBlue
  (-blue [l]
    (bit-and l 0xff))

  IHue
  (-hue [l]
    (hue (hsl l)))

  ISaturation
  (-saturation [l]
    (saturation (hsl l)))

  ILightness
  (-lightness [l]
    (lightness (hsl l))))


;;; String

(def ^:private hex-re
  "Regex for matching a hex color with optional leading
  \"#\". Captures the hex value."
  #"#?((?:[\da-fA-F]{3}){1,2})")

(let [;; Rgb channel range (0 - 255)
      c "\\s*([01]?\\d?\\d|2[0-4]\\d|25[0-5])\\s*"
      ;; Hue
      h "\\s*(\\d+)\\s*"
      ;; Saturation
      s "\\s*(\\d|[1-9]\\d|100)%\\s*"
      ;; Lightness
      l "\\s*(\\d|[1-9]\\d|100)%\\s*"
      ;; Alpha channel range (0 - 1)
      a "\\s*(0?(?:\\.\\d+)?|1(?:\\.0)?|\\d+\\.\\d+(?:[eE][-+]?\\d+)?)\\s*"]
  (def ^:private rgb-re
    "Regular expression mathing a CSS rgb color value. Captures each
  argument."
    (re-pattern (str "rgb\\(" c "," c "," c "\\)")))

  (def ^:private rgba-re
    "Regular expression matching a CSS rgba color value. Captures each
  argument."
    (re-pattern (str "rgba\\(" c "," c "," c "," a "\\)")))

  (def ^:private hsl-re
    "Regular expression matching a CSS hsl color value. Caputres each
  argument."
    (re-pattern (str "hsl\\(" h "," s "," l "\\)")))

  (def ^:private hsla-re
    "Regular expression matching a CSS hsla color value. Captures each
  argument."
    (re-pattern (str "hsla\\(" h "," s "," l "," a "\\)"))))

(defn ^Rgb hex-str->rgb
  "Convert a CSS hex value to instance of Rgb."
  [^String s]
  (if-let [[_ ^String h] (re-matches hex-re s)]
    (let [[r g b] (if (= (.-length h) 3)
                    (mapv #(js/parseInt (str % %) 16)
                          h)
                    (mapv #(js/parseInt % 16)
                          (re-seq #"[\da-fA-F]{2}" h)))]
      (Rgb. r g b))
    (throw (ex-info (str "Invalid hex string: " (pr-str s))
                    {:given s
                     :expected `(~'re-matches ~hex-re ~s)}))))

(defn ^Rgb rgb-str->rgb
  "Convert a CSS rgb value to an instance of Rgb."
  [^String s]
  (if-let [[_ & ns] (re-matches rgb-re s)]
    (let [[r g b] (map #(js/parseInt ^String %) ns)]
      (Rgb. r g b))
    (throw (ex-info (str "Invalid rgb string: " (pr-str s))
                    {:given s
                     :expected `(~'re-matches ~rgb-re ~s)}))))

(defn ^Rgba rgba-str->rgba
  "Convert a CSS rgba value to an instance of Rgba."
  [^String s]
  (if-let [[_ r g b a] (re-matches rgba-re s)]
    (let [[r g b] (map #(js/parseInt ^String %) [r g b])
          a (js/parseFloat a)]
      (Rgba. r g b a))
    (throw (ex-info (str "Invalid rgb string: " (pr-str s))
                    {:given s
                     :expected `(~'re-matches ~rgba-re ~s)}))))

(defn ^Hsl hsl-str->hsl
  "Convert a CSS hsl value to an instance of Hsl."
  [^String s]
  (if-let [[_ h s l] (re-matches hsl-re s)]
    (let [[h s] (map #(js/parseInt ^String %) [h s])
          l (js/parseFloat l)]
      (Hsl. h s l))
    (throw (ex-info (str "Invalid hsl string: " (pr-str s))
                    {:given s
                     :expected `(~'re-matches ~hsl-re ~s)}))))

(defn ^Hsla hsla-str->hsla
  "Convert a CSS hsla value to an instance of Hsla."
  [^String s]
  (if-let [[_ h s l a] (re-matches hsla-re s)]
    (let [[h s] (map #(js/parseInt ^String %) [h s])
          l (js/parseFloat l)
          a (js/parseFloat a)]
      (Hsla. h s l a))
    (throw (ex-info (str "Invalid hsla string: " (pr-str s))
                    {:given s
                     :expected `(~'re-matches ~hsla-re ~s)}))))

(def
  ^{:doc "Convert a CSS color name to hex."}
  color-name->hex
  {"aliceblue" "#f0f8ff"
   "antiquewhite" "#faebd7"
   "aqua" "#00ffff"
   "aquamarine" "#7fffd4"
   "azure" "#f0ffff"
   "beige" "#f5f5dc"
   "bisque" "#ffe4c4"
   "black" "#000000"
   "blanchedalmond" "#ffebcd"
   "blue" "#0000ff"
   "blueviolet" "#8a2be2"
   "brown" "#a52a2a"
   "burlywood" "#deb887"
   "cadetblue" "#5f9ea0"
   "chartreuse" "#7fff00"
   "chocolate" "#d2691e"
   "coral" "#ff7f50"
   "cornflowerblue" "#6495ed"
   "cornsilk" "#fff8dc"
   "crimson" "#dc143c"
   "cyan" "#00ffff"
   "darkblue" "#00008b"
   "darkcyan" "#008b8b"
   "darkgoldenrod" "#b8860b"
   "darkgray" "#a9a9a9"
   "darkgreen" "#006400"
   "darkgrey" "#a9a9a9"
   "darkkhaki" "#bdb76b"
   "darkmagenta" "#8b008b"
   "darkolivegreen" "#556b2f"
   "darkorange" "#ff8c00"
   "darkorchid" "#9932cc"
   "darkred" "#8b0000"
   "darksalmon" "#e9967a"
   "darkseagreen" "#8fbc8f"
   "darkslateblue" "#483d8b"
   "darkslategray" "#2f4f4f"
   "darkslategrey" "#2f4f4f"
   "darkturquoise" "#00ced1"
   "darkviolet" "#9400d3"
   "deeppink" "#ff1493"
   "deepskyblue" "#00bfff"
   "dimgray" "#696969"
   "dimgrey" "#696969"
   "dodgerblue" "#1e90ff"
   "firebrick" "#b22222"
   "floralwhite" "#fffaf0"
   "forestgreen" "#228b22"
   "fuchsia" "#ff00ff"
   "gainsboro" "#dcdcdc"
   "ghostwhite" "#f8f8ff"
   "gold" "#ffd700"
   "goldenrod" "#daa520"
   "gray" "#808080"
   "green" "#008000"
   "greenyellow" "#adff2f"
   "honeydew" "#f0fff0"
   "hotpink" "#ff69b4"
   "indianred" "#cd5c5c"
   "indigo" "#4b0082"
   "ivory" "#fffff0"
   "khaki" "#f0e68c"
   "lavender" "#e6e6fa"
   "lavenderblush" "#fff0f5"
   "lawngreen" "#7cfc00"
   "lemonchiffon" "#fffacd"
   "lightblue" "#add8e6"
   "lightcoral" "#f08080"
   "lightcyan" "#e0ffff"
   "lightgoldenrodyellow" "#fafad2"
   "lightgray" "#d3d3d3"
   "lightgreen" "#90ee90"
   "lightgrey" "#d3d3d3"
   "lightpink" "#ffb6c1"
   "lightsalmon" "#ffa07a"
   "lightseagreen" "#20b2aa"
   "lightskyblue" "#87cefa"
   "lightslategray" "#778899"
   "lightslategrey" "#778899"
   "lightsteelblue" "#b0c4de"
   "lightyellow" "#ffffe0"
   "lime" "#00ff00"
   "limegreen" "#32cd32"
   "linen" "#faf0e6"
   "magenta" "#ff00ff"
   "maroon" "#800000"
   "mediumaquamarine" "#66cdaa"
   "mediumblue" "#0000cd"
   "mediumorchid" "#ba55d3"
   "mediumpurple" "#9370db"
   "mediumseagreen" "#3cb371"
   "mediumslateblue" "#7b68ee"
   "mediumspringgreen" "#00fa9a"
   "mediumturquoise" "#48d1cc"
   "mediumvioletred" "#c71585"
   "midnightblue" "#191970"
   "mintcream" "#f5fffa"
   "mistyrose" "#ffe4e1"
   "moccasin" "#ffe4b5"
   "navajowhite" "#ffdead"
   "navy" "#000080"
   "oldlace" "#fdf5e6"
   "olive" "#808000"
   "olivedrab" "#6b8e23"
   "orange" "#ffa500"
   "orangered" "#ff4500"
   "orchid" "#da70d6"
   "palegoldenrod" "#eee8aa"
   "palegreen" "#98fb98"
   "paleturquoise" "#afeeee"
   "palevioletred" "#db7093"
   "papayawhip" "#ffefd5"
   "peachpuff" "#ffdab9"
   "peru" "#cd853f"
   "pink" "#ffc0cb"
   "plum" "#dda0dd"
   "powderblue" "#b0e0e6"
   "purple" "#800080"
   "red" "#ff0000"
   "rosybrown" "#bc8f8f"
   "royalblue" "#4169e1"
   "saddlebrown" "#8b4513"
   "salmon" "#fa8072"
   "sandybrown" "#f4a460"
   "seagreen" "#2e8b57"
   "seashell" "#fff5ee"
   "sienna" "#a0522d"
   "silver" "#c0c0c0"
   "skyblue" "#87ceeb"
   "slateblue" "#6a5acd"
   "slategray" "#708090"
   "slategrey" "#708090"
   "snow" "#fffafa"
   "springgreen" "#00ff7f"
   "steelblue" "#4682b4"
   "tan" "#d2b48c"
   "teal" "#008080"
   "thistle" "#d8bfd8"
   "tomato" "#ff6347"
   "turquoise" "#40e0d0"
   "violet" "#ee82ee"
   "wheat" "#f5deb3"
   "white" "#ffffff"
   "whitesmoke" "#f5f5f5"
   "yellow" "#ffff00"
   "yellowgreen" "#9acd32"})

(defn str->color
  "Attempt to parse a color from a string."
  [^String s]
  (cond
   (re-matches hex-re s)
   (hex-str->rgb s)

   (re-matches rgb-re s)
   (rgb-str->rgb s)

   (re-matches rgba-re s)
   (rgba-str->rgba s)

   (re-matches hsl-re s)
   (hsl-str->hsl s)

   (re-matches hsla-re s)
   (hsla-str->hsla s)

   :else
   (if-let [h (-> (string/lower-case s)
                  (string/replace #"[^a-z]" "")
                  (color-name->hex))]
     (rgb h)
     (throw (ex-info (str "Unable to convert " (pr-str s) " to a color")
                     {:given s
                      :expected `(~'or (~'re-matches ~hex-re ~s)
                                       (~'re-matches ~rgb-re ~s)
                                       (~'re-matches ~rgba-re ~s)
                                       (~'re-matches ~hsl-re ~s)
                                       (~'re-matches ~hsla-re ~s)
                                       (~'color-name->hex ~s))})))))

(extend-type string
  IRgb
  (-rgb [s]
    (rgb (str->color s)))

  IRgba
  (-rgba [s]
    (rgba (str->color s)))

  IHsl
  (-hsl [s]
    (hsl (str->color s)))

  IHsla
  (-hsla [s]
    (hsla (str->color s)))

  IRed
  (-red [s]
    (.-r ^Rgb (-rgb s)))

  IGreen
  (-green [s]
    (.-g ^Rgb (-rgb s)))

  IBlue
  (-blue [s]
    (.-b ^Rgb (-rgb s)))

  IHue
  (-hue [s]
    (.-h ^Hsl (-hsl s)))

  ISaturation
  (-saturation [s]
    (.-s ^Hsl (-hsl s)))

  ILightness
  (-lightness [s]
    (.-l ^Hsl (-hsl s)))

  IAlpha
  (-alpha [s]
    (.-a ^Rgba (-rgba s))))

;;; Keyword

;; TODO: Error messages here will appear in the String
;; implementation. This is potentially confusing.
(extend-type Keyword
  IRgb
  (-rgb [k]
    (rgb (name k)))

  IRgba
  (-rgba [k]
    (-rgba (name k)))

  IHsl
  (-hsl [k]
    (-hsl (name k)))

  IHsla
  (-hsla [k]
    (-hsla (name k)))

  IAlpha
  (-alpha [k]
    (-alpha (-rgba k)))

  IRed
  (-red [k]
    (-red (-rgb k)))

  IGreen
  (-green [k]
    (-green (-rgb k)))

  IBlue
  (-blue [k]
    (-blue (-rgb k)))

  IHue
  (-hue [k]
    (-hue (-hsl k)))

  ISaturation
  (-saturation [k]
    (-saturation (-hsl k)))

  ILightness
  (-lightness [k]
    (-lightness (-hsl k))))

;;; Vector

(defn bound-fn
  "Return a function which takes a number and returns it if (<= a n
  b), returns a if (< n 0), or b if (< b n)."
  {:private true}
  [a b]
  (fn [n]
    (if (<= a b)
      (min b (max n a))
      (min a (max n b)))))

(def
  ^{:arglists '([n])
    :private true}
  rgb-bounded
  (bound-fn 0 255))

(def
  ^{:arglists '([n])
    :private true}
  saturation-bounded
  (bound-fn 0 100))

(def
  ^{:arglists '([n])
    :private true}
  lightness-bounded
  (bound-fn 0 100))

(def
  ^{:arglists '([n])
    :private true}
  alpha-bounded
  (bound-fn 0 1))


(extend-type PersistentVector
  ;; The wrapper functions are used on purpose so we can leverage
  ;; validation on the return values.
  IRgb
  (-rgb [v]
    (Rgb. (red v) (green v) (blue v)))

  IRgba
  (-rgba [v]
    (Rgba. (red v) (green v) (blue v) (alpha v)))

  IHsl
  (-hsl [v]
    (Hsl. (hue v) (saturation v) (lightness v)))

  IHsla
  (-hsla [v]
    (Hsla. (hue v) (saturation v) (lightness v) (alpha v)))

  IAlpha
  (-alpha [v]
    (get v 3 1.0))

  IRed
  (-red [v]
    (rgb-bounded (get v 0 0)))

  IGreen
  (-green [v]
    (rgb-bounded (get v 1 0)))

  IBlue
  (-blue [v]
    (rgb-bounded (get v 2 0)))

  IHue
  (-hue [v]
    (get v 0 0))

  ISaturation
  (-saturation [v]
    (saturation-bounded (get v 1 0)))

  ILightness
  (-lightness [v]
    (lightness-bounded (get v 2 0))))
