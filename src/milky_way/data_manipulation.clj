(ns milky-way.data-manipulation
  (:require
    [milky-way.utils :as utils]
    [clojure.java.io :as io]
    [camel-snake-kebab.core :refer [->kebab-case]]
    [clojure.core.async :as async :refer [>!! <!!]]
    [clojure.java.io :as io]
    [clojure.spec.alpha :as s]
    [clojure.string :as str]
    [taoensso.timbre :as timbre :refer [spy]])
  (:import (java.io BufferedReader PushbackReader FileReader IOException)
           (org.apache.commons.lang3 StringUtils)))

(set! *warn-on-reflection* true)

(defn chop-end [a-string] (StringUtils/chop ^java.lang.String a-string))

(defn chop-start [a-string] (.substring ^java.lang.String  a-string 1))

;; (defmacro inspect  )

(defn split [a-string sep]
  (vec
    (StringUtils/split
      ^java.lang.String a-string ^java.lang.String sep)))

(defn build-vector-parser
  ([] (build-vector-parser identity))
  ([parser-fns]
   (let [parser (if (coll? parser-fns)
                 (fn [x]  (mapv #(%2 %1) x (cycle parser-fns)))
                 (fn [x]  (mapv #(%2 %1) x (cycle [parser-fns]))))]
     (fn [a-string]
       (-> a-string  (chop-start) (chop-end) (split " ") (parser))))))


(defn- reshape-string [st]
  "Convert strings to valid clojure keywords"
  (-> st (StringUtils/replace "(" "<")
      (StringUtils/replace  ")" ">")
      (StringUtils/replace  "/" "-per-")
      (StringUtils/replace  "/" "-per-")
      (StringUtils/replace  ":" ".")
      (->kebab-case)
      (keyword)))

;;StringUtils/split does not use regex hence is much faster
(defn split-csv-line [a-line]
 (vec (StringUtils/split  ^java.lang.String a-line  ",")))

(defn csv-head->labels [head]
 (mapv reshape-string (split-csv-line head)))

;TODO  change to {:labels {:strings :keywords}

(defn csv->data [[head & tail]]
  {:labels {:strings (split-csv-line head)
            :keywords   (csv-head->labels head)}
   :data (map split-csv-line  tail)})

;;TODO check if this should be a record

(defn array-zipmap [array-1 array-2]
  (apply array-map (interleave  array-1 array-2)))

(defn csv-stream [file-name & {:keys [labels] :or {labels "keyword"}}]
  (let   [ raw-data (utils/chunked-line-seq file-name)
          csv-data (csv->data raw-data)
          labels (if (= "STRING" (str/upper-case (name labels)))
                   (-> csv-data :labels :strings)
                   (-> csv-data :labels :keywords))]
    {:labels (:labels csv-data)
     :data   (map #(array-zipmap labels % ) (:data csv-data))}))


#_(def test-csv-stream
   (update  (csv-stream utils/csv-test) :data #(mapv vals %)))

(s/fdef csv-stream
        :args (s/and  (s/cat :file-name string? :labels (some-fn keyword? string? symbol?))))

(defn save-csv [file-name  {data :data {:keys [ strings keywords]} :labels}]
  (async/thread
    (try
      (timbre/info (str "exporting to file " file-name))
      (with-open [w (io/writer file-name)]
        (binding  [*out* w]
          (print (str/join  ","  strings))
          (doseq [line data]
            (print \newline)
            (print (str/join   ","  line)))))
      (timbre/info (str  file-name "successfully written to disk"))
      (catch Exception e (timbre/info (str "Error writting data to disk " (.getMessage e)))))))


#_(s/fdef csv-stream
        :args (s/and  (s/cat :file-name string? :labels (some-fn keyword? string? symbol?))))
