(ns milky-way.functions
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.core.async :as async :refer [>!! <!!]]
            [clojure.set :refer [rename-keys]]
            [cheshire.core :refer [generate-stream]]
            [taoensso.timbre :as timbre :refer [spy]]
            [taoensso.tufte :as tufte :refer (defnp p profiled profile)]
            [milky-way.data-manipulation :refer [split-csv-line  csv-stream save-csv build-vector-parser csv-head->labels array-zipmap ]]
            [milky-way.utils :refer [file-print chunked-line-seq] :as utils])
  (:import (java.io BufferedReader PushbackReader FileReader IOException )
           (org.apache.commons.math3.analysis.function Log Tanh Tan Sin Cos Gaussian)
           (org.apache.commons.math3.transform FastFourierTransformer DftNormalization TransformType)
           (org.apache.commons.math3.util FastMath)
           (org.apache.commons.math3.util MathArrays)))

(set! *warn-on-reflection* true)


(-> "config.edn" slurp read-string :timbre-config eval timbre/merge-config!)


#_(let [data-folder (-> "config.edn" slurp read-string :data)]
  (def domains (rest (file-seq (io/file data-folder "domains"))))
  (def spiral-sets (rest (file-seq (io/file data-folder "spiral-sets"))))
  (def characteristic-functions (rest  (file-seq (io/file data-folder "characteristic-functions"))))
  (def gaussian-2d-functions (rest (file-seq (io/file data-folder "2d-gaussian-functions"))))
  (def data-path data-folder))


(def PI (FastMath/PI))


(def model& (atom {:Domain nil}))


(def step 0.003)
(def start 0.001)
(def finish (- PI step))
(def opts {:A 800 :B 0.4 :N 5})
(def angles [0 (/ PI 2) PI (/ (* 3 PI) 2)])



(declare build-gaussian)
(declare spiral)
(declare ring)


(def functions  {:Log  #(FastMath/log %)
                 :Tan #(FastMath/tan %)
                 :Tanh  #(FastMath/tan %)
                 :Sin #(FastMath/sin %)
                 :Cos #(FastMath/cos %)
                 :Sqrt #(FastMath/sqrt %)
                 :Gaussian build-gaussian
                 :FFT (FastFourierTransformer. DftNormalization/STANDARD)
                 :Spiral spiral
                 :Ring ring
                 :Abs  #(FastMath/abs (double   %))
                 :Sec  #(/ -1 (FastMath/pow ^double (FastMath/cos %) 2))
                 :Sech #(/ -1 (FastMath/pow ^double (FastMath/cosh %) 2))
                 :**  #(FastMath/pow ^double % 2)
                 :*n   #(FastMath/pow ^double % ^double %2)
                 :Convolve   #(MathArrays/convolve (long-array %) (long-array %2))})

;; (timbre/info (type (Gaussian. 0 1)) )
(defn build-gaussian
  ([] (build-gaussian {}))
  ([{:keys [norm mean standard-deviation]
     :or {mean 0 standard-deviation 1}}]
   (let [gaussian (if norm (Gaussian. norm mean standard-deviation)
                      (Gaussian. mean standard-deviation))]
     (fn [x]
       (.value ^org.apache.commons.math3.analysis.function.Gaussian gaussian ^double (double  x))))))


(defn build-2d-gaussian
  ([] (build-2d-gaussian {}))
  ([{:keys [standard-deviation A]
     :or {standard-deviation 100 mean 0 A 0} :as opts}]
   (let [sqrt-2 (FastMath/sqrt  2)
         gaussian-1d (build-gaussian (assoc opts :standard-deviation  (/ standard-deviation sqrt-2)))]
     (fn [[x y]]
       (*  (gaussian-1d x)  (gaussian-1d y))))))


(def log
  (let [fun  (:Log functions)]
    (fn [x] (fun (double x)))))


(def tan
  (let [fun  (:Tan functions)]
    (fn [x] (fun (double x)))))


(def tanh
  (let [fun  (:Tanh functions)]
    (fn [x] (fun  (double x)))))


(def cos
  (let [fun  (:Cos functions)]
    (fn [x] (fun (double x)))))


(def  sin
  (let [fun  (:Sin functions)]
    (fn [x] (fun (double x)))))


(def sec
  (let [fun  (:Sec functions)]
    (fn [x] (fun (double x)))))


(def **
  (let [fun  (:** functions)]
    (fn [x] (fun (double x)))))


(def *4
  (let [fun  (:*n functions)]
    (fn [x] (fun (double x) 4))))


(def *n
  (let [fun  (:*n functions)]
    (fn [x n] (fun (double x) n))))


(def fast-fourier-transform
  (let [fft-fun  (:FFT functions)]
    (fn [the-seq & {:keys [transform-type] :or {transform-type :forward}}]
      (let [transform-name  (clojure.string/upper-case (name transform-type))
            least-quadratic (let [item-number (count the-seq)]
                              (loop [a 2]
                                (let [a' (* 2 a)]
                                  (if (> a' item-number)
                                    a (recur a')))))
            the-array (double-array (map :y (subvec the-seq 0 least-quadratic)))]
        (let [the-transform-type  (cond  (= "FORWARD" transform-name)
                                         TransformType/FORWARD
                                         (= "INVERSE" transform-name)
                                         TransformType/INVERSE
                                         :default "invalid")]
          (if (= the-transform-type "invalid")
            (timbre/info
             "Invalid transform type. Please insert either \"forward\" or \"inverse\" ")
            (map #(assoc % :y (.getReal ^org.apache.commons.math3.complex.Complex  %2))
                 the-seq (vec (.transform ^org.apache.commons.math3.transform.FastFourierTransformer fft-fun the-array ^org.apache.commons.math3.transform.TransformType the-transform-type)))))))))


(defn convolve [seq-1 seq-2]
  (let [transform-1 (fast-fourier-transform seq-1)
        transform-2 (fast-fourier-transform seq-2)]
    (timbre/info "point-wise multiplication started")
    (let [point-wise-multiplication
          (mapv  #(update % :y (fn [y] (+ y (:y %2))))  transform-1 transform-2)]
      (timbre/info "point wise multiplication finished")
      (async/thread
        (timbre/info "convolution started")
        (let  [convolved   (fast-fourier-transform point-wise-multiplication :transform-type :inverse)]
          (timbre/info "convolution finished")
          convolved)))))

;; (swap! model& assoc :Convolution )

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



(defn bar-set [&  [x opts width density]]
  (let [b   (spiral x opts) abs (:Abs functions)]
    (for [k  (range 0  (abs  b) step)]
      (for [phi (range (* -1  (/ width 2)) (/ width 2) (/ 1 density))]
        [phi (* -1   k)]))))


(defn bar-set-complement [&  [x opts start density]]
  (let [abs (:Abs functions)
        b (abs  (spiral x opts))]
    (for [k  (range start b step)]
      (for [phi (range (* -1 b) (* -1 start)  (/ 1 density))]
        [phi (* -1  k)]))))



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
  (concat (bar-set start opts width density)
          (for [phi (range start finish step)]
            (for [ksi (range (* -1  (/ width 2)) (/ width 2) (/ 1 density))]
              (normal-vector  phi opts  ksi)))))


;; (functions/multispiral-complement-set [0 (/ PI 2) PI (/ (* 3 PI) 2)] 16 100 2)
(defn single-spiral-complement-set [outer inner density]
  (concat (bar-set-complement start opts inner density)
          (for [phi (range start finish step)]
            (for [ksi (concat  (range (* -1 outer) (* -1 inner)  (/ 1 density))
                               (range   inner  outer  (/ 1 density)))]
              (normal-vector  phi opts  ksi)))))


(defn- rotate-spiral [θ the-spiral]
  (let [rotation-matrix [[(cos θ) (* -1 (sin θ))]
                         [(sin θ) (cos θ)]]
        [[a b] [c d]] rotation-matrix]
    (map (fn [x] (map (fn [[x y]] [(+ (* x  a) (* y b))
                                   (+ (* x  c) (* y d))]) x)) the-spiral)))


(defn multispiral-set  [angles width density]
  (let [the-spiral  (single-spiral-set width density)]
    (apply concat  (mapcat #(rotate-spiral % the-spiral)  angles))))


(defn multispiral-complement-set  [angles inner outer density]
  (let [the-spiral (single-spiral-complement-set outer inner density)]
    (apply concat (mapcat #(rotate-spiral % the-spiral)  angles))))


(defn basic-spiral [file-name]
  (async/thread
    (file-print file-name
                [(vec (multispiral-set angles 32 2))
                 (vec (multispiral-complement-set angles 16 100 2))])))


(defn build-basic-spiral  []
  (async/thread
    (timbre/info "generating spiral")
    (let [the-spiral [(vec (multispiral-set angles 32 2))
                      (vec (multispiral-complement-set angles 16 100 2))]
          _ (timbre/info "spiral generated succesfully ")]
      the-spiral)))

(defn build-characteristic  [the-vec]
  (when-let [[the-set the-complement] the-vec ]
     (fn [x]
       (cond (some #(x) the-set) 1
             (some #(x) the-complement) 0
             :default "NaN"))))


(defn ->spiral-file [ch file-name]
  (async/take!
    ch
    (fn [[a b]] (save-csv file-name
                          {:data (vec (sort (concat a b)))
                           :labels {:strings ["spiral-set"]}}))))


(defn load-set [file-name]
  (let [csv-seq  (csv-stream file-name)
        parser  (build-vector-parser  #(Double/parseDouble %) ) ]
    (update csv-seq :data #(map parser %))))

;

#_(defn load-set-chunked [file-name]
    (let [[head & tail]  (chunked-line-seq file-name)
          types [str str double double double double double double double double double double double double str str double  ]
          labels  (csv-head->labels head)
          csv->data (comp (map split-csv-line)
                          (map  #(map (fn [t x] (t x)) (cycle [str]) %))
                          (map (partial array-zipmap labels)))]
      (sequence csv->data tail)))


(defn save-characteristic [ch file-name]
  (async/take! ch
               (fn[[x y]]
                 (let [data  (vec (concat
                                    (map #(vector % 1) x)
                                    (map #(vector % 0) y)))]
                   (save-csv  file-name {:data data :labels
                                         {:strings ["x" "y"]}
                                         :keyords [:x :y] })))))


(defn build-2d-gaussian-points
  ([the-seq] (build-2d-gaussian-points the-seq {}))
  ([the-seq opts]
   (let [the-gaussian (build-2d-gaussian opts)]
       (async/thread
         (try
           (timbre/info (str "generation of gaussian function points started"))
           (let [the-gaussian
                 (mapv (fn [x] (array-map :x x :y (the-gaussian x))) the-seq)
                 _  (timbre/info (str "gaussian was generated succesfuly"))])
           the-gaussian
           (catch Exception e (timbre/info (str "Error loading Data" (.getMessage e)))))))))


(defn save-gaussian [ch file-name]
  (async/take! ch
               (fn [a]
                 (let [data  (mapv #(vector (:x %) (:y %) )  a)]
                   (save-csv  file-name {:data data :labels
                                         {:strings ["x" "y"]}
                                         :keywords [:x :y] })))))


#_(let [points  (:data (load-set "basic-spiral"))
        ch (build-2d-gaussian-points points)]
  (save-gaussian ch "gaussian-points"))


(defn sort-by-x [input output]
(timbre/info (str "sorting started"))
  (async/thread
       (try
         (with-open [in (PushbackReader. (io/reader (io/input-stream (io/file  input)) ))  ]
           (timbre/info (str "opening file " input)  )
           (let [the-set  (vec (edn/read {:eof :theend} in)) ]
             (timbre/info (str input " proccesed"))
             (with-open [w (io/writer output)]
                 (print-dup (sort-by :x the-set) w ))))
         (timbre/info (str "file-sorted"))
         (catch Exception e (timbre/info (str "Error" (.getMessage e)))))) )


(defn seq->fft [input output & {:keys [transform-type]}]
  (async/thread
    (try
      (with-open [w (io/writer output)]
        (binding  [*out* w]
          (pr (fast-fourier-transform input :transform-type transform-type))))
      (timbre/info (str "fast fourier transform calculated succesfully"))
      (catch Exception e (timbre/info (str "Error" (.getMessage e)))))) )


#_(defn point-wise-multiplication [file-1 file-2  fout ]
    (timbre/info (str "Opening files " file-1 " and " file-2))
    (try
      (with-open [in-1 (PushbackReader. (io/reader (io/file file-1) ))
                  in-2  (PushbackReader. (io/reader (io/file file-2) ))]
        (timbre/info (str "Starting point-wise multiplication"))
        (with-open [w (io/writer fout)]
          (print-dup (mapv (fn [a b] {:y (* (:y a)  (:y b)) :x (:x a) })
                           (read {:eof :theend} in-2)
                           (read {:eof :theend} in-1)) w))
        (timbre/info "Point-wise multiplication finished"))
      (catch Exception e (timbre/info (str "Error" (.getMessage e))))))


#_(defn convolve-from-files [file-1 file-2 & {fout :print-file}]
  (with-open [in-1 (PushbackReader. (io/reader (io/input-stream (io/file file-1)) ))
              in-2  (PushbackReader. (io/reader (io/input-stream (io/file file-2)) ))]
    (let [seq-1 (edn/read {:eof :theend} in-1)
          seq-2 (edn/read {:eof :theend} in-2) ]
      (let [transform-1 (fast-fourier-transform seq-1)
            transform-2 (fast-fourier-transform seq-2)]
        (timbre/info "point-wise multiplication started")
        (let [point-wise-multiplication
              (mapv  #(update % :y (fn [y] (+ y (:y %2))))  transform-1 transform-2)]
          (timbre/info "point wise multiplication finished")
          (async/thread
            (timbre/info "convolution started")
            (if fout
              (try
                (with-open [w (io/writer fout)]
                  (binding  [*out* w]
                    (pr (fast-fourier-transform point-wise-multiplication :transform-type :inverse)))
                  (timbre/info "convolution finished"))
                (catch IOException  e (timbre/info (str "Couldn't operate on " fout ", error: " (.getMessage e))))))
            (let  [convolved   (fast-fourier-transform point-wise-multiplication :transform-type :inverse)]
              (timbre/info "convolution finished")
              convolved)))))))


#_(defn point-wise-multiplication [file-1 file-2  fout ]
  (timbre/info (str "Opening files " file-1 " and " file-2))
  (try
    (with-open [in-1 (PushbackReader. (io/reader (io/file file-1) ))
                in-2  (PushbackReader. (io/reader (io/file file-2) ))]
        (timbre/info (str "Starting point-wise multiplication"))
          (with-open [w (io/writer fout)]
            (print-dup (map (fn [{x :x y1 :y}  {y2 :y} ] {:y (* y1 y2) :x x  })
                             (read {:eof :theend} in-2)
                             (read {:eof :theend} in-1)) w))
          (timbre/info "Point-wise multiplication finished"))
    (catch Exception e (timbre/info (str "Error" (.getMessage e))))))


#_(defn point-wise-multiplication [file-1 file-2  fout ]
  (async/thread
    (timbre/info (str "Opening files " file-1 " and " file-2))
    (try (let   [in-1 (PushbackReader. (io/reader (io/input-stream (io/file file-1)) ))
                 in-2  (PushbackReader. (io/reader (io/input-stream (io/file file-2)) ))]
           (with-open [w (io/writer fout)]
               (timbre/info "starting multiplication")
               (print-dup (map   (fn [a b] (* (:y a) (:y b) ) )  (edn/read {:eof :theend} in-2) (edn/read {:eof :theend} in-1))  w )))
         (timbre/info "Point-wise multiplication finished")
         (catch Exception e (timbre/info (str "Error" (.getMessage e)))))))

