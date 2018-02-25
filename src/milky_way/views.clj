(ns milky-way.views
  (:import [org.apache.commons.math3.util FastMath])
  (:require
   [milky-way.functions :as functions]
   [quil.core :as q]
   [quil.middleware :as m]))

(def PI (FastMath/PI))


(defn uuid [] (str (java.util.UUID/randomUUID)))

(def step 0.001)

(def start 0.001)

(def finish (- PI step))


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

(def opts {:A 800 :B 0.4 :N 5})


; define function which draws spiral


(defn draw-spiral [&  {:keys [save] :or {save false}}]
  (q/background 231 5 100)
  (q/with-translation [(/ (q/width) 2) (/ (q/height) 2)]
    (q/line  -800 0 800 0)
    (q/line  0 -800 0 800)
    (let [the-set (functions/multispiral-complement-set [0 (/ PI 2) PI (/ (* 3 PI) 2)] 16 100 2)]
      (doseq [[x y] the-set]
        (q/point  x y))))
  #_(q/save  (str (uuid) "---milky-way.png"))
  (when save (q/save  (str (uuid) "-milky-way.png"))))

#_(defn draw-spiral-complement [&  {:keys [save] :or {save false}}]
  ; move origin point to centre of the sketch
  ; by default origin is in the left top corner
    (q/background 231 5 100)
    (q/with-translation [(/ (q/width) 2) (/ (q/height) 2)]
      (q/line  -800 0 800 0)
      (q/line  0 -800 0 800)
      (doseq [φ (range start finish step)]
        (doseq  [ξ (concat (range  (- 100) (- 15) 0.5) (range (+ 15) (+ 100) 0.5))]
          (let [[x y] (functions/normal-vector  φ opts  ξ)]
            (q/point  x y)
            (q/point  (* -1 x) (* -1 y))
            (q/with-rotation [(/ PI 2)]
              (q/point  x y)
              (q/point  (* -1 x) (* -1 y)))))))
    (when save (q/save  (str (uuid) "-milky-way.png"))))
