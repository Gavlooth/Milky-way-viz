(ns milky-way.views
  (:import [org.apache.commons.math3.util FastMath])
  (:require
   [milky-way.functions :refer [spiral fat-spiral t-opts]]
   [quil.core :as q]
   [quil.middleware :as m]))

(defn uuid [] (str (java.util.UUID/randomUUID)))

(def step 0.001)

(def start 0.001)

(def finish (- FastMath/PI step))



(defn draw-horizontal-line
  "Draws a horizontal line on the canvas at height h"
  [h]
  (q/line 10 h (- (q/width) 20) h)
  (q/stroke 255 h)
  (q/line 10 (+ h 4) (- (q/width) 20) (+ h 4)))


(defn draw-horizontal-line
  "Draws a vertical line on the canvas at width w"
  [w]
  (q/stroke 255 w))


(defn setup []
  (q/no-loop)
  (q/smooth)
  (q/stroke-weight 0.8)
  (q/color-mode :hsb 360 100 100 1.0))


(defn draw []
 (let [the-spiral   (spiral {:A 800 :B 0.4 :N 16})]
  ; move origin point to centre of the sketch
  ; by default origin is in the left top corner
  (q/background 231 5 100)
  (q/with-translation [(/ (q/width) 2) (/ (q/height) 2)]
    (q/line  -800 0 800 0)
    (q/line  0 -800 0 800)
    (doseq [phi (range start finish step)]
      (let [[x y] (the-spiral phi)]
        (q/point  x y)
        (q/point  (* -1 x) (* -1 y))
        (q/point  (* 1 y) (* -1 x))
        (q/point  (* -1 y) (* 1 x))))
    #_(doseq [phi (range start (* finish 0.5) step)]
        (let [[x y] (the-spiral phi)]
          (q/point  (* 1 y) (* -1 x))
          (q/point  (* -1 y) (* 1 x))))
   (q/save  (str (uuid) "-milky-way.png")))))



(defn draw-fat-spiral []
 (let [the-fat-spiral   (fat-spiral t-opts)]
  ; move origin point to centre of the sketch
  ; by default origin is in the left top corner
  (q/background 231 5 100)
  (q/with-translation [(/ (q/width) 2) (/ (q/height) 2)]
    (q/line  -800 0 800 0)
    (q/line  0 -800 0 800)
    (doseq [phi (range start finish step)]
      (let [points (the-fat-spiral phi)]
        (doseq [[x y] points]
          (q/point  x y)
          (q/point  (* -1 x) (* -1 y))
          (q/point  (* 1 y) (* -1 x))
          (q/point  (* -1 y) (* 1 x)))))
    (q/save  (str (uuid) "-milky-way.png")))))

