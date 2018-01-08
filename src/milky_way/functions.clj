(ns milky-way.functions
  (:import [org.apache.commons.math3.analysis.function Log Tanh Tan Sin Cos]))

(def functions  {:Log (Log.) :Tan (Tan.) :Tanh (Tanh.) :Sin (Sin.) :Cos (Cos.)})
(defn log [x]
  (.value (:Log functions) (double x)))

(defn tan [x]
  (.value (:Tan functions) (double  x)))

(defn tanh [x]
  (.value (:Tanh functions) (double  x)))

(defn sin [x]
  (.value (:Sin functions) (double  x)))

(defn cos [x]
  (.value (:Cos functions) (double  x)))


(defn spiral-galaxy
  [x &  {:keys [A B N] :or {A 1 B 1 N 1}}]
  (let [r      (/ A (log   (* B (tan (/ x (* 2 N))))))]
    [(* r (sin x)) (* r (cos x))]))

(defn ring-galaxy
  [x &  {:keys [A B N] :or {A 1 B 1 N 1}}]
  (let [r      (/ A (log   (* B (tanh (/ x (* 2 N))))))]
    [(* r (sin x)) (* r (cos x))]))



