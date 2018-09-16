(ns milky-way.data
  (:require [infix.macros :refer [infix from-string]])
  (:import (org.apache.commons.math3.analysis.function  Gaussian)
           (org.apache.commons.math3.transform FastFourierTransformer DftNormalization)))



(def generate-gaussian (fn [& {:keys [norm mean standard-deviation]
                               :or {mean 0 standard-deviation 1}}]
                        (if norm
                          (Gaussian. norm mean standard-deviation)
                          (Gaussian. mean standard-deviation))))


(def fast-fourier-transformer (FastFourierTransformer. DftNormalization/STANDARD))

;; (map :name (:members (clojure.reflect/reflect  fast-fourier-transformer)))
