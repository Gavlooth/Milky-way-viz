(ns milky-way.data
  (:require [infix.macros :refer [infix from-string]]
            [milky-way.functions :as functions :refer [sqr  sqrt pow]])
  (:import (org.apache.commons.math3.transform FastFourierTransformer DftNormalization)
           (org.apache.commons.math3.util MathArrays)))


(defn distance-2d [[x1 y1] [y2 x2]]
  (sqrt (+ (sqr (- x1 x2)) (sqr (- y1 y2)))))

(defn norm-2d [x]
  (distance-2d x [0 0]))

(def generate-gaussian (fn [& {:keys [norm mean standard-deviation]
                               :or {mean 0 standard-deviation 1}}]
                        (if norm
                          (Gaussian. norm mean standard-deviation)
                          (Gaussian. mean standard-deviation))))


(def gaussian-2d
 (let [the-gaussian (generate-gaussian)]
  #(.value  the-gaussian ^double (/ (norm-2d %) 200))))

(defn sample-the-galaxy [n t-opts]
   (let [D1 (functions/sample-spiral-2d n t-opts)
         D2 (functions/sample-spiral-2d-C (* 2 n) t-opts)
         R1 (double-array (concat (map gaussian-2d D1)
                                  (map gaussian-2d D2)))
         R2 (double-array (concat (repeat (count  D1) 1)
                                  (repeat (count D2) 0)))]
     (MathArrays/convolve R1 R2)))

(def t-opts  {:A 800
              :B 1
              :N 16
              :Width 15})

(vec (sample-the-galaxy 100 t-opts))
