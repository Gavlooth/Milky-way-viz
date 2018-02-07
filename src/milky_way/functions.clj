(ns milky-way.functions
  (:import [org.apache.commons.math3.analysis.function Log Tanh Tan Sin Cos Gaussian]
           [org.apache.commons.math3.util FastMath]
           [org.apache.commons.math3.util MathArrays]))


(def PI (FastMath/PI))


(def step 0.001)
(def start 0.001)
(def finish (- PI step))
(def opts {:A 800 :B 0.4 :N 5})
(def angles [0 (/ PI 2) PI (/ (* 3 PI) 2)])


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


(def model-container& (atom {:Domain nil}))

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


(defn single-spiral-set [width density]
  (for [phi (range start finish step)]
    (for [ksi (range (* -1  (/ width 2)) (/ width 2) (/ 1 density))]
      (normal-vector  phi opts  ksi))))


(defn single-spiral-complement-set [outer inner density]
  (for [phi (range start finish step)]
    (for [ksi (cat  (range (* -1 outer) (* -1 inner)  (/ 1 density))
                    (range   inner  outer  (/ 1 density)))]
      (normal-vector  phi opts  ksi))))


(defn- rotate-spiral [θ the-spiral]
  (let [rotation-matrix [[(cos θ) (* -1 (sin θ))]
                         [(sin θ) (cos θ)]]
        [[a b] [c d]] rotation-matrix]
    (map (fn [x] (map (fn [[x y]] [(+ (* x  a) (* y b))
                                   (+ (* x  c) (* y d))]) x)) the-spiral)))


(defn multispiral-set  [angles width density]
  (let [the-spiral (single-spiral-set width density)]
    (mapcat #(rotate-spiral % the-spiral)  angles)))


(defn multispiral-complement-set  [angles inner outer density]
  (let [the-spiral (single-spiral-complement-set outer inner density)]
    (mapcat #(rotate-spiral % the-spiral)  angles)))

(defn  generate-characteristic  []
  (when-let [[the-set the-complement] (:Domain @model-container&)]
    (fn [x]
      (cond (some #(x) the-set) 1
            (some #(x) the-complement) 0
            :default "NaN"))))

(defn spiral-set-1 []
  (swap! model-container& assoc  :Domain
          [(multispiral-set angles 30 2)
           (multispiral-complement-set angles 15 100 2)]))


(generate-characteristic)

