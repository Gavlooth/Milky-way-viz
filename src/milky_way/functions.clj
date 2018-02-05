(ns milky-way.functions
  (:import [org.apache.commons.math3.analysis.function Log Tanh Tan Sin Cos Gaussian]
           [org.apache.commons.math3.util FastMath]
           [org.apache.commons.math3.util MathArrays]))


(def step 0.001)
(def start 0.001)
(def finish (- FastMath/PI step))


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
                 :*n   #(FastMath/pow % %2)
                 :convolve   #(MathArrays/convolve (long-array %) (long-array %2))})


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
  (defn *4 [x] (fun (double x) 4))
  (defn *n [x n] (fun (double x) n)))


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


(defn ortho-normalize-vector [[x y]]
  (when (or (not= 0 x) (not= 0 y))
    (let [x' (/ x  (*n (+ (** y) (** x)) 0.5))
          y' (/ y  (*n (+ (** y) (** x)) 0.5))]
      [(* -1 y') x'])))


(defn normal-vector
  ([x opts] (normal-vector x opts 1))
  ([x opts l]
   (let  [[a b] (parametric-spiral x opts)
          the-point (parametric-spiral-velocity  x opts)
          [w z]  (ortho-normalize-vector the-point)]
     [(+ a (* l  w)) (+ b (* l z))])))

(defn  fat-spiral-set
  [{:keys [width point-density] :as opts :or
    {width 10 point-density 1}}]
  (for [phi (range start finish step)]
    (for [ksi (range  (* -1 (/ width 2))
                     (/ width 2)
                     (/ width  (* point-density 100)))]
      (normal-vector  phi opts  ksi))))

(defn  fat-spiral-complement-set
  [{:keys [width point-density complement-range] :as opts :or
    {width 10 point-density 1 complement-range 100}}]
  (for [phi  (range start finish step)]
    (vec
      (concat
        (for [ksi (range (* -1 complement-range)
                         (* -1 (/ width 2))
                         (/ width  (* point-density 100)))]
          (normal-vector  phi opts  ksi))
        (for
          [ksi (range (/ width 2) complement-range (/ width (* point-density 100)))]
          (normal-vector  phi opts  ksi))))))



(defn skata [] (take 10 (fat-spiral-complement-set {})))


