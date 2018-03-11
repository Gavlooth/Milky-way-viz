(ns milky-way.functions
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.core.async :as async :refer [>!! <!!]]
            [clojure.set :refer [rename-keys]]
            [cheshire.core :refer [generate-stream]]
            [taoensso.timbre :as timbre :refer [spy]]
            [taoensso.tufte :as tufte :refer (defnp p profiled profile)]  )
  (:import (java.io BufferedReader PushbackReader FileReader IOException )
           (org.apache.commons.math3.analysis.function Log Tanh Tan Sin Cos Gaussian)
           (org.apache.commons.math3.transform FastFourierTransformer DftNormalization TransformType)
           (org.apache.commons.math3.util FastMath)
           (org.apache.commons.math3.util MathArrays)))

(set! *warn-on-reflection* true)


(-> "config.edn" slurp read-string :timbre-config eval timbre/merge-config!)


(let [data-folder (-> "config.edn" slurp read-string :data)]
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

(defn char-seq
  [^java.io.BufferedReader rdr]
  (let [chr (.read rdr)]
    (when-not (= chr -1)
      (cons (char chr) (lazy-seq  (char-seq rdr))))))

(defn file-print [file-name output & {:keys [append]} ]
  (try
    (with-open [w (io/writer (io/file  file-name) :append append)]
      (binding  [*out* w]
        (timbre/info  (str  "Writting file " file-name))
        (pr output ))
      (timbre/info (str  "Finished writting " file-name)))
    (catch Exception  e (timbre/info (str "error: " (.getMessage e))))))

(defn parse-file [file-1]
  (try
    (with-open [input  (-> file-1 (io/file) (FileReader.) ( BufferedReader.) (PushbackReader.))]
      (timbre/info (str "coersing file " file-1 " to sequence")  )
      (let [the-seq (seq (repeatedly (partial edn/read {:eof :theend} input)))]
        (timbre/info (str file-1 " parsed!") the-seq)))
    (catch Exception  e (timbre/info (str "error: " (.getMessage e))))))

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
       (.value ^org.apache.commons.math3.analysis.function.Gaussian gaussian  ^double (double  x))))))


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


(defn build-characteristic  []
  (when-let [[the-set the-complement] (:Domain @model&)]
    (memoize
     (fn [x]
       (cond (some #(x) the-set) 1
             (some #(x) the-complement) 0
             :default "NaN")))))


#_(defn build-characteristic-graph  [file-name]
  {:pre [string? file-name]}
  (when-let [[the-set the-complement]  (:Domain @model&)]
    (async/thread
      (try
        (with-open [w (io/writer file-name)]
          (binding  [*out* w]
            (pr (vec (concat (map #(hash-map :x % :y 1) the-set)
                                 (map #(hash-map :x % :y 0)
                                      the-complement))))))
        (timbre/info (str "characteristic was exported succesfuly"))
        (catch Exception e (timbre/info (str "Error " (.getMessage e))))))
    (timbre/info (str "generation of characteristic function points started"))))



#_(defn build-characteristic-graph-from-file [domain-file file-name]
  {:pre [string? file-name string? domain-file]}
  (async/thread
    (timbre/info (str "generation of characteristic function points started"))
    (try
      (with-open [input (PushbackReader. (io/reader (io/input-stream (io/file  domain-file)) ))  ]
        (timbre/info (str "opening file " domain-file  )  )
        (let [[the-set the-complement] (vec (edn/read {:eof :theend} input))]
          (timbre/info (str domain-file " proccesed"))
          (with-open [w (io/writer file-name)]
            (binding  [*out* w]
              (pr (vec (concat (map #(hash-map :x % :y 1) the-set)
                                    (map #(hash-map :x % :y 0)
                                         the-complement))))))))
      (timbre/info (str "characteristic was exported succesfuly"))
      (catch Exception e (timbre/info (str "Error" (.getMessage e)))))))

#_(defn build-2d-gaussian-graph
  ([file-name] (build-2d-gaussian-graph file-name {}))
  ([file-name opts]
   {:pre [string? file-name map? opts]}
   (when-let [[the-set the-complement]  (:Domain @model&)]
     (let [domain (vec (concat the-set the-complement))
           the-gaussian (build-2d-gaussian opts)]
       (async/thread
         (try
           (with-open [w (io/writer file-name)]
             (binding  [*out* w]
               (pr (mapv (fn [x] (hash-map
                                          :x x :y (the-gaussian x)))
                                 domain))))
           (timbre/info (str "gaussian was exported succesfuly"))
           (catch Exception e (timbre/info (str "Error loading Data" (.getMessage e))))))
       (timbre/info (str "generation of gaussian function points started"))))))

#_(defn build-2d-gaussian-graph-from-file
  ([domain-file file-name] (build-2d-gaussian-graph-from-file domain-file file-name {}))
  ([ domain-file file-name opts]
   {:pre [string? domain-file string? file-name map? opts]}
   (let [the-gaussian (build-2d-gaussian opts)]
     (timbre/info (str "generation of gaussian function points started"))
     (async/thread
       (try
         (with-open [input (PushbackReader. (io/reader (io/input-stream (io/file  domain-file)) ))  ]
           (timbre/info (str "opening file " domain-file  )  )
           (let [[the-set the-complement]  (vec (edn/read {:eof :theend} input))
                  domain (vec (concat the-set the-complement))]
             (timbre/info (str domain-file " proccesed"))
             (with-open [w (io/writer file-name)]
               (binding  [*out* w]
                 (pr (mapv (fn [x] (hash-map :x x :y (the-gaussian x))) domain ))))))
         (timbre/info (str "gaussian was exported succesfuly"))
         (catch Exception e (timbre/info (str "Error" (.getMessage e)))))))))


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

#_(sort-by-x "fft-gaussian-01.edn" "fft-gaussian.edn"  )

(defn save-model  [file-name & {:keys [Key] :or {Key :Domain}}]
  {:pre [string? file-name keyword? Key]}
  (async/thread
    (try
      (timbre/info (str "exporting to file " file-name))
      (with-open [w (io/writer file-name)]
        (binding  [*out* w]
          (pr (get @model& Key))))
      (timbre/info (str  file-name "successfully written to disk"))
      (catch Exception e (timbre/info (str "Error writting data to disk " (.getMessage e)))))))


(defn load-model [file-name & {:keys [Key] :or {Key :Domain}}]
  (let [the-file (io/file file-name)]
    (async/take!
     (async/thread
       (timbre/info (str "loading " (.getName ^java.io.File the-file)))
       (with-open [input (PushbackReader. (io/reader the-file))]
         (into []  (edn/read {:eof :theend} input))))
     (fn [x]
       (try
         (swap! model& assoc Key x)
         (timbre/info (str "data from " (.getName ^java.io.File the-file) " was loaded succesfuly"))
         (catch Exception e (timbre/info  (str "Error loading Data" (.getMessage e)))))))))


(defn file->fft [input output & {:keys [transform-type]}]
  (timbre/info (str "applying fast fourier transform"))
  (async/thread
    (try
      (with-open [in (PushbackReader. (io/reader (io/input-stream (io/file  input)) ))  ]
        (timbre/info (str "opening file " input)  )
        (let [the-set (vec (edn/read {:eof :theend} in)) ]
          (timbre/info (str input " proccesed"))
          (with-open [w (io/writer output)]
            (binding  [*out* w]
              (pr (fast-fourier-transform the-set :transform-type transform-type))))))
      (timbre/info (str "fast fourier transform calculated succesfully"))
      (catch Exception e (timbre/info (str "Error" (.getMessage e)))))) )


(defn  split-series [input]
  (timbre/info (str "spliting " input))
  (async/thread
    (try
      (with-open [in (PushbackReader. (io/reader (io/file  input)))  ]
        (timbre/info (str "opening file " input)  )
        (let [the-set (edn/read {:eof :theend} in)
              the-size (count the-set)]
          (timbre/info (str input " proccesed"))
          (loop [part  (partition (int (/ the-size 5) )  the-set) i 0 ]
            (let  [[head & tail] part]
              (when head
                (with-open [w (io/writer (str  input  "--" i))]
                  (print-dup head w))
                (timbre/info (str "file "  input  "--" i " written on disk"))
                (recur tail (inc i)))))
          (timbre/info (str "file " input " splited succesfully"  ))))
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




#_(async/thread  (doseq [i [0 1 2 3 4]]  (point-wise-multiplication (str  "/mnt/10E3-FF70/fft-gaussian.edn--" i)
                                                                   (str  "/mnt/10E3-FF70/fft-characteristic.edn--" i)
                                                                   (str  "/mnt/10E3-FF70/FFT-spiral.edn--"i))))
#_(file->fft "characteristic-01.edn" "fft-characteristic-01.edn")
#_(with-open [input  (-> file-1 (io/file) (FileReader.) ( BufferedReader.) (PushbackReader.)) #_(PushbackReader. (io/reader "gaussian-01.edn"))]
    (let [edn-seq (repeatedly (partial edn/read {:eof :theend} input))]
      (timbre/info (first  (first edn-seq)))))
#_(do (basic-spiral))
#_(load-model "spiral-01.edn" :Key :Domain)
#_(load-model "gaussian-01.edn" :Key :Gaussian)
#_(let [_ (load-model (first  gaussian-2d-functions) :Key :Gaussian)
        _ (load-model (first  characteristic-functions) :Key :Characteristic)]
    #_(convolve (:Gaussian @model&)  (:Characteristic @model&)))
#_(build-2d-gaussian-graph-from-file "spiral-01.edn"  "gaussian-01.edn"   )
#_(def a (async/thread
           (do (timbre/info "start of proccessing")
               (let [b (forward-fft (:Gaussian @model&))]
                 (timbre/info "end of proccessing") b))))

#_(point-wise-multiplication "fft-gaussian-01.edn" "fft-characteristic-01.edn"  "pre-final.edn")
#_(build-gaussian-graph-from-file "spiral-01.edn" "gaussian-01.edn" )
#_(convolve-from-files "characteristic-01.edn" "gaussian-01.edn" :print-file "convolution.edn" )
#_(-> domain-file (io/file) (FileReader.) ( BufferedReader.) (PushbackReader.))




#_(async/thread
    (timbre/info "started")
    (doseq [i [0 1 2 3 4]]
      (with-open [the-spiral  (PushbackReader. (io/reader (io/file (str  "/mnt/10E3-FF70/FFT-spiral.edn--" i)) ))]
        (with-open [w (io/writer "/mnt/10E3-FF70/fft-the-spiral.edn" :append true)]
          (binding [*out* w]
            (doseq [x (read  {:eof :theend} the-spiral) ]
              (pr x)))
          (timbre/info (str "file no " i " written succesfully"))))))


#_(async/thread
    (timbre/info "started")
    (with-open [w (io/writer "/mnt/10E3-FF70/fft-function.edn" )]
      (binding [*out* w]
        (print  \[)
        (io/copy (io/file "/mnt/10E3-FF70/fft-the-spiral.edn") w )
        (print \]))))


#_(async/thread
    (timbre/info "started")
    (with-open [the-file (PushbackReader. (io/reader (io/file (str  "/mnt/10E3-FF70/function.edn" ))))]
      (with-open [w (io/writer "/mnt/10E3-FF70/function.json"  ) ]
        (generate-stream (map #(rename-keys % {:x "point" :y "probability"}) (read  {:eof :theend} the-file)) w)))
    (timbre/info "operation completed"))


#_(def the-file (char-seq (io/reader (str  "/mnt/10E3-FF70/fft-the-spiral.edn" ))) )
#_(async/thread (timbre/info (count (filter #{\(} the-file ))))
#_(async/thread  (file->fft "/mnt/10E3-FF70/fft-function.edn" "/mnt/10E3-FF70/function.edn" :transform-type "inverse"  ))
#_(with-open [the-spiral  (PushbackReader. (io/reader (io/file "/mnt/10E3-FF70/inverse-the-spiral.edn" ) ))]
      (timbre/info (count (read  {:eof :theend} the-spiral))))
#_(async/thread
    (timbre/info "started")
    (with-open [the-spiral  (PushbackReader. (io/reader (io/file "/mnt/10E3-FF70/fft-the-spiral-final.edn" ) ))]
      (timbre/info (count (read  {:eof :theend} the-spiral))))
    (with-open [the-spiral  (PushbackReader. (io/reader (io/file "/mnt/10E3-FF70/fft-gaussian.edn" ) ))]
      (timbre/info (count (read  {:eof :theend} the-spiral))))
    (with-open [the-spiral  (PushbackReader. (io/reader (io/file "/mnt/10E3-FF70/fft-characteristic.edn" ) ))]
      (timbre/info (count (read  {:eof :theend} the-spiral)))))
#_(async/thread
    (timbre/info "started")
    (with-open [the-spiral  (PushbackReader. (io/reader (io/file "/mnt/10E3-FF70/fft-the-spiral.edn" ) ))]
      (timbre/info (count (read  {:eof :theend} the-spiral))))
    (with-open [the-spiral  (PushbackReader. (io/reader (io/file "/mnt/10E3-FF70/fft-the-spiral-final.edn" ) ))]
      (timbre/info (count (read  {:eof :theend} the-spiral)))))
#_(async/thread
    (timbre/info "started")
    (doseq [i [0 1 2 3 4]]
      (with-open [the-spiral  (PushbackReader. (io/reader (io/file (str  "/mnt/10E3-FF70/FFT-spiral.edn--" i)) ))]
        (with-open [w (io/writer "/mnt/10E3-FF70/fft-the-spiral-2.edn" :append true)]
           (doseq [x (read  {:eof :theend}  the-spiral)]
             (print-dup x  w))
         (timbre/info (str "file no " i " written succesfully"))))))
