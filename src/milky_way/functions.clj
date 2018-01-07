(ns milky-way.functions
  (:import [org.apache.commons.math3.analysis.function Log Tanh Tan Sin Cos]))

(def functions [(Log.) (Tan.) (Tanh.) (Sin.) (Cos.)])
(defn log [x]
  (.value (first functions) (double x)))

(defn tan [x]
  (.value (second functions) (double  x)))

(defn tanh [x]
  (.value (get functions 2) (double  x)))

(defn sin [x]
  (.value (get functions 3) (double  x)))

(defn cos [x]
  (.value (get functions 4) (double  x)))


(defn spiral-galaxy
  [x &  {:keys [A B N] :or {A 1 B 1 N 1}}]
  (let [r      (/ A (log   (* B (tan (/ x (* 2 N))))))]
    [(* r (sin x)) (* r (cos x))]))

(defn ring-galaxy
  [x &  {:keys [A B N] :or {A 1 B 1 N 1}}]
  (let [r      (/ A (log   (* B (tanh (/ x (* 2 N))))))]
    [(* r (sin x)) (* r (cos x))]))



