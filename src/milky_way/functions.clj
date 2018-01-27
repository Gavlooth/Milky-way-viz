(ns milky-way.functions
  (:import [org.apache.commons.math3.analysis.function Log Tanh Tan Sin Cos Gaussian]
           [org.apache.commons.math3.util FastMath])
  (:require [infix.macros :refer [infix from-string base-env]]))

(declare generate-gaussian)
(declare spiral)
(declare ring)

(def functions  {:Log  #(FastMath/log %)
                 :Tan #(FastMath/tan %)
                 :Tanh  #(FastMath/tan %)
                 :Sin #(FastMath/sin %)
                 :Cos #(FastMath/cos %)
                 :Gaussian generate-gaussian
                 :Spiral spiral
                 :Ring ring
                 :Sec  #(/ -1 (FastMath/pow (FastMath/cos %) 2))
                 :Sech #(/ -1 (FastMath/pow (FastMath/cosh %) 2))
                 :**  #(FastMath/pow % 2)
                 :*n   #(FastMath/pow % %2)})


(defn generate-gaussian
  [& {:keys [norm mean standard-deviation]
      :or {mean 0 standard-deviation 1}}]
  (if norm
    (Gaussian. norm mean standard-deviation)
    (Gaussian. mean standard-deviation)))


(let [fun  (:Log functions)]
  (defn log [x] (fun (double x))))


(let [fun  (:Tan functions)]
  (defn tan [x] (fun (double x))))


(let [fun  (:Tanh functions)]
  (defn tanh  [x] (fun  (double x))))


(let [fun  (:Cos functions)]
  (defn cos [x] (fun (double x))))


(let [fun  (:Sin functions)]
  (defn sin [x] (fun (double x))))

(let [fun  (:Sec functions)]
  (defn sec [x] (fun (double x))))

(let [fun  (:** functions)]
  (defn ** [x] (fun (double x))))


(let [fun  (:*n functions)]
  (defn *4 [x] (fun (double x) 4)))

(/ A (log   (* B (tan (/ x (* 2 N))))))
(def spiral
  (fn  [x {:keys [A B N] :or {A 1 B 1 N 1}}]
    (infix  A / (log / ( B * tan ( x / (2 * N)))) ) ))


(defn spiral [x &  {:keys [A B N] :or {A 1 B 1 N 1}}]
  (/ A (log   (* B (tan (/ x (* 2 N)))))))

(def anti-spiral
  (fn   [x   {:keys [A B N] :or {A 1 B 1 N 1}}]
     (infix    (log / ( B * tan ( x / (2 * N)))) / A)))

(defn anti-spiral
  [x &  {:keys [A B N] :or {A 1 B 1 N 1}}]
  (/ (log   (* B (tan (/ x (* 2 N))))) A))

(def anti-spiral-derivative
  (fn [x   {:keys [A B N] :or {A 1 B 1 N 1}}]
    (infix  ((2* N * (sec (x / (2 * N))))   /
             (A * (tan (x / (2 * N))))  )))  )

(defn anti-spiral-derivative
  [x &  {:keys [A B N] :or {A 1 B 1 N 1}}]
  (/   (* 2  N (sec (/ x (* 2 N))))
       (*  A (tan (/ x (* 2 N))))))

(/ (* -1  (anti-spiral-derivative x :A A :B B :N N))
     (** (anti-spiral x :A A :B B :N N)))
;;Fix the constant values
(def spiral-dirivative
 (fn  [x  {:keys [A B N] :or {A 1 B 1 N 1} :as params}]
   (infix (anti-spiral-derivative x params)
          /
          ( ( anti-spiral x  params) *n 2))))

(defn spiral-dirivative [x & {:keys [A B N] :or {A 1 B 1 N 1}}]
  (/ (* -1  (anti-spiral-derivative x :A A :B B :N N))
     (** (anti-spiral x :A A :B B :N N))))


(defn  parametric-spiral
  [x & {:keys [A B N] :or {A 1 B 1 N 1}}]
  (let [r  (spiral x :A A :B B :N N)]
    [(* r (sin x)) (* r (cos x))]))


(defn parametric-spiral-velocity  [x & {:keys [A B N] :or {A 1 B 1 N 1}}]
  (let [r  (spiral x :A A :B B :N N)
        r' (spiral-dirivative x :A A :B B :N N)]
    [(+  (* r' (sin x))
         (* r  (cos x)))
     (-  (* r' (cos x))
         (* r  (sin x)))]))

(defn parametric-spiral-normal-line [x & {:keys [A B N] :or {A 1 B 1 N 1}}]
  (let [[x' y']    (parametric-spiral-velocity x :A A :B B :N N)
        slop (/ x' y')
        [x0 y0] (parametric-spiral x :A A :B B :N N)]
    (fn [w]
      [w (+   (* slop (- w  x0)) y0)])))

;; (.println System/out x0)


