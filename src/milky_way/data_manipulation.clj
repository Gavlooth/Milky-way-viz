(ns milky-way.data-manipulation
  (:require
    [clojure.java.io :as io]
    [camel-snake-kebab.core :refer [->kebab-case]]
    [clojure.core.async :as async :refer [>!! <!!]]
    [clojure.java.io :as io]
    [clojure.spec.alpha :as s]
    [clojure.string :as str]

            [taoensso.timbre :as timbre :refer [spy]]
    [taoensso.tufte :as tufte :refer (defnp p profiled profile)]
    [kixi.stats.core :refer  [standard-deviation correlation correlation-matrix]])
  (:import (java.io BufferedReader PushbackReader FileReader IOException )

           (org.apache.commons.lang3 StringUtils)) )

(set! *warn-on-reflection* true)

(def ->?double
  (s/conformer
   (fn [x]
     (cond
       (integer? x) x
       (string? x)  (try
                      (Double/parseDouble x)
                      (catch Exception _
                        :clojure.spec.alpha/invalid))
       :else :clojure.spec.alpha/invalid))))


(def ->?integer
  (s/conformer
   (fn  [x]
     (cond
       (integer? x) x
       (string? x)  (try
                      (Integer/parseInt (StringUtils/removeEnd  x ".0"))
                      (catch Exception _
                        :clojure.spec.alpha/invalid))
       :else :clojure.spec.alpha/invalid))))

;; (clojure.reflect/reflect StringUtils)

;; Load the resources. TODO move data from resources to data/ folder


(defn- reshape-string [st]
  "Convert strings to valid clojure keywords"
  (-> st (StringUtils/replace "(" "<")
         (StringUtils/replace  ")" ">")
         (StringUtils/replace  "/" "-per-")
         (StringUtils/replace  "/" "-per-")
         (StringUtils/replace  ":" ".")
         (->kebab-case)
         (keyword)))

(defn split-csv-line [a-line]
 (vec (StringUtils/split   a-line  "," )))


(defn csv-head->labels [head]
(map reshape-string (split-csv-line head)) )

(defn csv->data [[head & tail]]
  {:string-labesl (split-csv-line head )
   :keyword-labels  (csv-head->labels head)
   :data tail })

;;maybe this should be a record
(defn csv-stream [file-name & {:keys [labels] :or {labels "keyword"}}]
  {:pre [string? file-name]}
  (let  [the-reader  (io/reader (io/input-stream (io/file file-name)))
         _ (timbre/info (type the-reader))
         csv-data (csv->data (line-seq the-reader))
         labels (if (= "STRING" (StringUtils/upperCase (name labels)))  (:string-labels csv-data)
                  (:keyword-labels csv-data))]
    {:labels (:string-labels csv-data)
      :data   (mapv (fn [x] (zipmap labels (split-csv-line  x))) (:data csv-data))
      :stream-closer #(.close ^java.io.BufferedReader the-reader)}))

#_(def pipa  (csv-stream (str "data/FL_insurance_sample.csv" ) ) )
#_(vec  (let [ [head & tail] pipa] (StringUtils/Split (first pipa) \, )  ))

#_(defn load-model [file-name & {:keys [Key] :or {Key :Domain}}]
  (let [the-file (io/file file-name)]
    (async/take!
     (async/thread
       (timbre/info (str "loading " (.getName ^java.io.File the-file)))
       (with-open [input (PushbackReader. (io/reader the-file))]
         (into []  (edn/read {:eof :theend} input))))
     (fn [x]
       (try
         (swap! model& assoc Key x)
         (timbre/info
           (str "data from " (.getName ^java.io.File the-file)
                " was loaded succesfuly"))
         (catch Exception e (timbre/info (str "Error loading Data" (.getMessage e)))))))))

