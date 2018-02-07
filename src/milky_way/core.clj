(ns milky-way.core
  (:require [quil.core :as q]
            [milky-way.views :as views]))

(defn run [& {:keys [draw]}]
  (q/defsketch milky-way
    :title "Milky way"
    :size [800 800]
    :setup views/setup
    :draw (or draw views/draw-spiral)))

