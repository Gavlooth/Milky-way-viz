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
 (let [the-vector (matrix/normalise  (matrix/mmul vector r-matrix))
       tips (drop (range 0 length  (/ length point-count)))]
  (map (partial matrix/mul [1 2])  tips)))


(defn fat-spiral [  {:keys [length point-count A B N]
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


;; ( (spiral-derivative t-opts) 2)
;; (def skata (fat-spiral))
;; (skata 2)

(defn ring-galaxy
  [x &  {:keys [A B N] :or {A 1 B 1 N 1}}]
  (let [r      (/ A (log   (* B (tanh (/ x (* 2 N))))))]
    [(* r (sin x)) (* r (cos x))]))
