(ns milky-way.functions
  (:require
    [infix.macros :refer [infix from-string]]
    [clojure.core.matrix :as matrix  :refer [mmul mul normalise]])
  (:import
    [org.apache.commons.math3.analysis.function Log Tanh Tan Sin Cos]
    [org.apache.commons.math3.util FastMath]))


(defmacro ->function [a-function]
  `(fn [x#] (. ~a-function ~(with-meta 'value {:tag (type a-function)})  (double x#))))

(def t-opts  {:length 2 :A 800 :B 0.4 :N 16 :point-count 20 :start 0 :end 2})

(def log (->function (Log.)))

(def tan  (->function (Tan.)))

(def tanh (->function (Tanh.)))

(def cos  (->function (Cos.)))

(def sin (->function (Sin.)))

(defn csc [x]
  (/ 1 (sin x)))

(defn convex-hull [[x1 y1] [x2 y2]]
 (fn [t] [(+ x1 (* t x2))  (+ y1 (* t y2))]))


(defn rotate-90 [a-vector]
  (mmul a-vector [[0 -1] [1 0]]))

(defn rotate-180 [a-vector]
 (mmul a-vector [[1 0] [0 1]]))

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
  ([](spiral {}))
  ([{:keys [A B N] :or {A 1 B 1 N 1}}]
   (let [r (parametric-radius-spiral A B N)]
    (fn [phi]
     [(infix  r(phi) *  sin(phi)) (infix  r(phi) * cos(phi))]))))


(defn spiral-derivative
  ([] (spiral-derivative {}))
  ([{:keys [A B N] :or {A 1 B 1 N 1}}]
   (let [r ( parametric-radius-spiral A B N)
         r' (parametric-radius-spiral-derivative A B N)]

    (fn [phi]
     [(infix r'(phi) * sin(phi)
             + r(phi) * cos(phi))
      (infix r'(phi) * cos(phi)
             - r(phi) * sin(phi))]))))

#_(defn orthonormal-spiral-vector
   ([phi] (orthonormal-spiral-vector phi {}))
   ([phi {:keys [A B N] :or {A 1 B 1 N 1} :as opts}]
    (let [f  (spiral opts)
          df (spiral-derivative opts)
          [x y] (f phi)
          [x' y'] (normalise  (rotate-90 (df phi)))]
     [[x y] [(+ x x')  (+ y y')]])))




(defn convex-hull [t] (fn [a b](+ (* a t)  (*  (- 1 t) b))))

(defn spiral-2d
  ([](spiral-2d {}))
  ([{:keys [A B N Width] :or {A 1 B 1 N 1 Width 1} :as opts}]
   (let [f (spiral opts)
         f' (spiral-derivative opts)]
     (fn [phi t]
      (let [g (convex-hull t)
            [x y :as X] (f phi)
            [x' y'] (matrix/add X  (mul  (normalise  (rotate-90 (f' phi))) Width))]
        [(g x x') (g y y')])))))



(defn spiral-2d-C
  ([](spiral-2d {}))
  ([{:keys [A B N Width] :or {A 1 B 1 N 1 Width 1} :as opts}]
   (let [f (spiral opts)
         f' (spiral-derivative opts)]
     (fn [phi t]
      (let [g (convex-hull t)
            [x y] (f phi)
            [x' y'] (mul  (normalise  (rotate-90 (f' phi))) Width)]
        [(g x x') (g y y')])))))
