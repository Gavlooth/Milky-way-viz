(ns milky-way.functions
  (:require
    [infix.macros :refer [infix from-string]]
    [clojure.core.matrix :as matrix])
  (:import
    [org.apache.commons.math3.analysis.function Log Tanh Tan Sin Cos]
    [org.apache.commons.math3.util FastMath]))


(defmacro ->function [a-function]
  `(fn [x#] (. ~a-function ~(with-meta 'value {:tag (type a-function)})  (double x#))))

(def r-matrix [[0 -1] [1 0]])

(def t-opts  {:A 800 :B 0.4 :N 16 :length 2 :point-count 20})

(def log (->function (Log.)))

(def tan  (->function (Tan.)))

(def tanh (->function (Tanh.)))

(def cos  (->function (Cos.)))

(def sin (->function (Sin.)))

(defn csc [x]
  (/ 1 (sin x)))


(defn parametric-radius-spiral [A B N]
 (fn [phi]
   (infix A / log ( B * tan ( phi / (2 * N))))))


(defn  parametric-radius-ring [A B N]
 (fn [phi]
   (infix A / log ( B * tan ( phi / (2 * N))))))


(defn parametric-radius-spiral-derivative [A B N]
   (fn [phi]
    (infix -1 * A * csc (phi / N)
           / (N * (log (B * tan(phi /(2 * N))) ** 2)))))


(defn spiral
  [{:keys [A B N] :or {A 1 B 1 N 1}}]
  (let [r (parametric-radius-spiral A B N)]
   (fn [phi]
    [(infix  r(phi) *  sin(phi)) (infix  r(phi) * cos(phi))])))


(defn spiral-derivative
  [{:keys [A B N] :or {A 1 B 1 N 1}}]
  (let [r ( parametric-radius-spiral A B N)
        r' (parametric-radius-spiral-derivative A B N)]

   (fn [phi]
    [(infix r'(phi) * sin(phi)
            + r(phi) * cos(phi))
     (infix r'(phi) * cos(phi)
            - r(phi) * sin(phi))])))


(defn orthogonal-line-segment [a-vector  {:keys [length point-count]
                                          :or {length 1 point-count 5}}]
 (let [the-vector (matrix/normalise  (matrix/mmul a-vector r-matrix))
       tips (rest (range 0 length  (float (/ length point-count))))]
  (map (partial matrix/mul the-vector)  tips)))


(defn fat-spiral [{:keys [length point-count A B N]
                   :or {length 1 point-count 5 A 1 B 1 N 1} :as opts}]
      (let [the-spiral (spiral opts)
            the-tangent (spiral-derivative opts)
            opts2 (dissoc opts :A :B :N)]
       (fn [phi]
        (let [point  (the-spiral phi)
              tangent (the-tangent phi)
              points (map matrix/add
                          (orthogonal-line-segment tangent opts)
                          (repeat point))]
             (cons point points)))))

;; (matrix/normalise  (matrix/mmul (q 4) r-matrix))
#_(def q (spiral-derivative t-opts))
#_(orthogonal-line-segment (q 4) t-opts)
#_(spiral {:A 800 :B 0.4 :N 16})
#_(def pipa (fat-spiral t-opts))
#_(pipa 4)
