(ns milky-way.views
  (:import [org.apache.commons.math3.util FastMath])
  (:require
   [milky-way.functions :as fns]
   [quil.core :as q]
   [quil.middleware :as m]))

(defn uuid [] (str (java.util.UUID/randomUUID)))

(def step 0.001)

(def start 0.001)

(def finish    (-   FastMath/PI step))

(defn setup []
  (q/no-loop)
  (q/smooth)
  (q/stroke-weight 0.2)
  (q/color-mode :hsb 360 100 100 1.0)

  #_(q/save  (str (uuid) "-milky-way.png")))


; define function which draws spiral
(defn draw []
  ; move origin point to centre of the sketch
  ; by default origin is in the left top corner
  (q/background 140 40 80)
  (q/with-translation [(/ (q/width) 2) (/ (q/height) 2)]
    ; parameter t goes 0, 0.01, 0.02, ..., p
    (q/scale 10)
    (doseq [x (range start finish step)]
      (apply q/point  (fns/spiral-galaxy   x  :A 2 :B 0.4 :N 16)))))




