(ns milky-way.functions
  (:import [org.apache.commons.math3.analysis.function Log Tanh Tan Sin Cos Gaussian]
           [FastMath]))


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
                 :DxTan  #(- 1 (FastMath/pow (FastMath/cos %) 2))
                 :DxTanh #(- 1 (FastMath/pow (FastMath/cosh %) 2))})


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

(let [fun  (:DxTan functions)]
  (defn dx-tan [x] (fun (double x))))


(defn spiral [x &  {:keys [A B N] :or {A 1 B 1 N 1}}]
  (/ A (log   (* B (tan (/ x (* 2 N)))))))

(defn anti-spiral
  [x &  {:keys [A B N] :or {A 1 B 1 N 1}}]
  (/ (log   (* B (tan (/ x (* 2 N))))) A))

(defn anti-spiral-derivative
  [x &  {:keys [A B N] :or {A 1 B 1 N 1}}]
  (/   (* 2 B N (dx-tan (/ x (* 2 N))))
       (/ (log   (* B (tan (/ x (* 2 N))))) A)))

(defn spiral-dirivative [x & {:keys [A B N] :or {A 1 B 1 N 1}}]
  (let [pow  (:pow functions)]
    (/ (-1  (anti-spiral-derivative x))
       (pow (anti-spiral x -2)))))


(defn anti-ring [x &  {:keys [A B N] :or {A 1 B 1 N 1}}]
  (/ (log   (* B (tanh (/ x (* 2 N))))) A))


(defn ring [x &  {:keys [A B N] :or {A 1 B 1 N 1}}]
  (/ A (log   (* B (tanh (/ x (* 2 N)))))))


(defn  parametric-spiral
  [x & {:keys [A B N] :or {A 1 B 1 N 1}}]
  (let [r  (spiral x :A A :B B :N N)]
    [(* r (sin x)) (* r (cos x))]))




(defn siral-velocity  [])


(defn parametric-ring
  [x &  {:keys [A B N] :or {A 1 B 1 N 1}}]
  (let [r  (ring x :A A :B B :N N)]
    [(* r (sin x)) (* r (cos x))]))



