(ns milky-way.core
  (:require [quil.core :as q]
            [milky-way.views :as views]))


(defn run [& args]
  (if-not (= (first args) :C)
   (q/defsketch milky-way
      :title "Milky way"
      :size [800 800]
      :setup views/setup
      :draw views/draw-2d)
   (q/defsketch milky-way
    :title "Milky way"
    :size [800 800]
    :setup views/setup
    :draw views/draw-2d-C)))

