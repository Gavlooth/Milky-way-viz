(ns milky-way.core
  (:require [quil.core :as q]
            [milky-way.views :as views]))


(defn run [& args]
  (q/defsketch milky-way
    :title "Milky way"
    :size [800 800]
    :setup views/setup
    :draw views/draw))

(defn run-2 [& args]
 (q/defsketch milky-way
   :title "Milky way"
   :size [800 800]
   :setup views/setup
   :draw views/draw))

(defn run-3 [& args]
 (q/defsketch milky-way
   :title "Milky way"
   :size [800 800]
   :setup views/setup
   :draw views/draw-fat-spiral-complements))

(defn run-4 [& args]
 (q/defsketch milky-way
   :title "Milky way"
   :size [800 800]
   :setup views/setup
   :draw views/draw-fat-spiral))
