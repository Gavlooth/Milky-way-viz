(ns milky-way.functions
  (:import [org.apache.commons.math3.analysis.function Log Tanh Tan Sin Cos Gaussian]))


(def functions  {:Log #(Log.)
                 :Tan #(Tan.)
                 :Tanh #(Tanh.)
                 :Sin #(Sin.)
                 :Cos #(Cos.)
                 :Gaussian  (fn [& {:keys [norm mean standard-deviation]
                                    :or {mean 0 standard-deviation 1}}])
                 (if norm
                   (Gaussian. norm mean standard-deviation)
                   (Gaussian. mean standard-deviation))})


(let [fun  ((:Log functions))]
  (defn log [x] (.value fun (double x))))


(let [fun  ((:Tan functions))]
  (defn tan [x] (.value  fun (double x))))


(let [fun  ((:Tanh functions))]
  (defn tanh  [x] (.value fun  (double x))))


(let [fun  ((:Cos functions))]
  (defn cos [x] (.value fun (double x))))


(let [fun  ((:Sin functions))]
  (defn sin [x] (.value fun (double x))))



(defn spiral-galaxy
  [x &  {:keys [A B N] :or {A 1 B 1 N 1}}]
  (let [r      (/ A (log   (* B (tan (/ x (* 2 N))))))]
    [(* r (sin x)) (* r (cos x))]))


(defn ring-galaxy
  [x &  {:keys [A B N] :or {A 1 B 1 N 1}}]
  (let [r      (/ A (log   (* B (tanh (/ x (* 2 N))))))]
    [(* r (sin x)) (* r (cos x))]))

