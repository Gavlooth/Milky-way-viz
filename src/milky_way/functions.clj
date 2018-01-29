(ns milky-way.functions
  (:import [org.apache.commons.math3.analysis.function Log Tanh Tan Sin Cos Gaussian]
           [org.apache.commons.math3.util FastMath]))


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

(defn spiral
  ([x] (spiral {}))
  ([x  opts]
   (let [{:keys [A B N]  :or {A 1 B 1 N 1}} opts]
     (/ A (log   (* B (tan (/ x (* 2 N)))))))))


(defn anti-spiral

  ([x] (anti-spiral {}))
  ([x   opts]
   (let [{:keys [A B N]  :or {A 1 B 1 N 1}} opts]
     (/ (log   (* B (tan (/ x (* 2 N))))) A))))


(defn anti-spiral-derivative
  ([x] (anti-spiral-derivative x {}))
  ([x opts]
   (let [{:keys [A B N]  :or {A 1 B 1 N 1}} opts]
     (/ (* 2  N (sec (/ x (* 2 N))))
        (*  A (tan (/ x (* 2 N))))))))


;;Fix the constant values


(defn spiral-dirivative [x  opts]
  (/ (* -1  (anti-spiral-derivative x opts))
     (** (anti-spiral x opts))))


(defn  parametric-spiral
  [x  opts]
  (let [r  (spiral x opts)]
    [(* r (sin x)) (* r (cos x))]))


(defn parametric-spiral-velocity  [x opts]
  (let [r  (spiral x opts)
        r' (spiral-dirivative x opts)]
    [(+  (* r' (sin x))
         (* r  (cos x)))
     (-  (* r' (cos x))
         (* r  (sin x)))]))

(defn parametric-spiral-tangent-line [x  opts]
  (let [[x' y']    (parametric-spiral-velocity x opts)
        slop (* -1 (/ y'  x' ))
        [x0 y0] (parametric-spiral x  opts)]
    (fn [w]
      [w (+   (* slop (- w  x0)) y0)])))

(defn parametric-spiral-normal-line [x  opts]
  (let [[x' y']    (parametric-spiral-velocity x opts)
        slop (* -1 (/ x' y'))
        [x0 y0] (parametric-spiral x  opts)]
    (fn [w]
      [w (+   (* slop (- w  x0)) y0)])))

(defn -test [x  opts]
  (let [[_ y0] (parametric-spiral x  opts)]
    (fn [w]
      [w  y0])))

