(ns milky-way.core
  (:require [quil.core :as q]
            [milky-way.views :as views])
  (:gen-class))


(defn -main  [& args]
  (q/defsketch milky-way
    :title "Milky way"
    :size [800 800]
    :setup views/setup
    :draw views/draw))

