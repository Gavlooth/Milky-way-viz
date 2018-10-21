(ns milky-way.functions
  (:require
    [infix.macros :refer [infix from-string]]
    [clojure.core.matrix :as matrix  :refer [mmul mul normalise]])
  (:import
    (org.apache.commons.math3.analysis.function Log Tanh Tan Sin Cos)
    (org.apache.commons.math3.util FastMath)
    (it.unimi.dsi.util XoShiRo256PlusRandom)
    #_(it.unimi.dsi Util)))


(def seed
 "Seed for reproduction"
 -2968425414217690972)

(def fast-rnd*
  "Fastest random number generetor thereis"
  (XoShiRo256PlusRandom. seed))

(defn next-rnd []
 (.nextDoubleFast ^it.unimi.dsi.util.XoShiRo256PlusRandom fast-rnd*))

(defn- sample-less-than [x]
 (let [end (- x 0.002)]
  #(+ 0.001 (* end (next-rnd)))))

(defn repeat-distinct [n f]
       (vec (distinct (repeatedly n f))))

(defn sample-repeat-distinct [n the-max]
   (repeat-distinct  n (sample-less-than the-max)))

(defn sample-n
 ([n]
  (sample-repeat-distinct n FastMath/PI))
 ([n width]
  (map vec
   (partition
     2
     (interleave
       (sample-n n)
       (sample-repeat-distinct n  width))))))


(defmacro ->function [a-function]
  `(fn [x#] (. ~a-function ~(with-meta 'value {:tag (type a-function)})  (double x#))))

(def t-opts  {:A 800
              :B 1
              :N 16
              :Width 15})

(def t-opts2  {:A 800
               :B 1
               :N 16
               :Width 5})

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

(defn- rotate-90-times [times a-vector]
  (mmul a-vector (reduce mmul (repeat times [[0 -1] [1 0]]))))

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


(defn convex-hull [t] (fn [a b](+ (* a t)  (*  (- 1 t) b))))

(defn  spiral-2d
  ([](spiral-2d {}))
  ([opts]
   (let [width (get opts :Width 2)
         f (spiral opts)
         f'(spiral-derivative opts)]
     (fn [phi t]
      (let [g (convex-hull t)
             X (f phi)
            [x2 y2] (rotate-90 (matrix/add X  (mul  (normalise  (rotate-90 (f' phi))) width)))
            [x1 y1] (rotate-90 (matrix/sub X  (mul  (normalise  (rotate-90 (f' phi))) width)))]
        [(g x1 x2) (g y1 y2)])))))


(defn  spiral-2d-C
  ([](spiral-2d-C {}))
  ([opts]
   (let [width (get opts :Width 1)
         f (spiral opts)
         f'(spiral-derivative opts)]
     (fn [phi t]
      (let [g (convex-hull t)
            X (f phi)
            [x2 y2] (rotate-90 (matrix/add X  (mul  (normalise  (rotate-90 (f' phi))) width)))
            [x1 y1] (matrix/sub X  (mul  (normalise  (rotate-90 (f' phi))) width))]
        [(g x1 x2) (g y1 y2)])))))



(defn sample-spiral-arm-2d
 #_([n] (sample-spiral-arm-2d n {}))
  [n opts]
  (let [the-spiral  (spiral-2d opts)
        points (sample-n n 1)]
    (map #(apply the-spiral %) points)))


(defn sample-spiral-2d
  ([n] (sample-spiral-2d n {}))
  ([n opts]
   (apply concat
    (for [i [1 2 3 4]]
       (map (partial rotate-90-times i) (sample-spiral-arm-2d n opts))))))


(defn sample-spiral-arm-2d-C
  [n opts]
  (let [the-spiral  (spiral-2d-C opts)
        points (sample-n n 1)]
    (map #(apply the-spiral %) points)))


(defn sample-spiral-2d-C
  ([n] (sample-spiral-2d-C n {}))
  ([n opts]
   (sample-spiral-arm-2d-C n opts)))

