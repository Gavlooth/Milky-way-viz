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
  (q/color-mode :hsb 360 100 100 1.0)

  #_(q/save  (str (uuid) "-milky-way.png")))


; define function which draws spiral
(defn draw []
  ; move origin point to centre of the sketch
  ; by default origin is in the left top corner
  (q/background 231 20 90)
  (q/with-translation [(/ (q/width) 2) (/ (q/height) 2)]

    (q/line  -800 0 800 0)
    (q/line  0 -800 0 800)
    (doseq [phi (range start finish step)]
      (let [[x y] (fns/spiral-galaxy   phi  :A 800 :B 0.4 :N 16)]
        (q/point   x y)
        (q/point  (* -1 x) (* -1 y))))
    (doseq [phi (range start (/ finish  3) step)]
      (let [[x y] (fns/spiral-galaxy   phi  :A 800 :B 0.4 :N 16)]
        (q/point  (* 1 y) (* -1 x))
        (q/point  (* -1 y) (* 1 x))))))


#_(defn annotate-x-axis []
  ;; Draw year labels
    (text-size 10)
    (text-align :center :top)
    (stroke-weight 1)
  ;; Use thin, gray lines to draw the grid
    (stroke 224)
    (doseq [year (range year-min year-max 10)]
      (let [x (map-range year year-min year-max plotx1 plotx2)]
        (text (str year) x (+ 10 ploty2))
        (line x ploty1 x ploty2))))

#_(defn annotate-y-axis []
  ;; Draw volume labels
  ;; Since we're not drawing the minor ticks, we would ideally
  ;; increase volume-interval to 10 and remove the modulo-10 check.
  ;; We keep it in to show how to produce figure 5.
    (text-align :right :center)
    (doseq [volume (range data-first (+ 1 data-max) volume-interval)]
      (let [y (map-range volume data-first data-max ploty2 ploty1)]
      ;; Commented out--the minor tick marks are too visually distracting
      ;; (stroke 128)
      ;; (line plotx1 y (- plotx1 2) y) ;; Draw minor tick
        (when (= 0 (mod volume 10)) ;; Draw major tick mark
          (stroke 0)
          (line plotx1 y (- plotx1 4) y)
          (text-align :right :center) ;; Center vertically
        ;; Align the "0" label by the bottom:
          (when (= volume data-first) (text-align :right :bottom))
          (text (str (ceil volume)) (- plotx1 10) y)))))

#_(defn draw-axis-labels []
  ;; Draw axis labels
    (text-size 13)
    (text-leading 15)
    (text-align :center :center)
    (text "Gallons\nconsumer\nper capita" 50 (/ (+ ploty1 ploty2) 2))
    (text (str (first (first milk-tea-coffee-data)))
          (/ (+ plotx1 plotx2) 2) (- (height) 25)))
