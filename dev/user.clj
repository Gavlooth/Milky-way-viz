(ns user
 (:require [clojure.tools.namespace.repl :refer [refresh refresh-all]]
           [milky-way.core :refer [run]]
           [milky-way.views :refer [draw]]))


(defn reset []
 (refresh)
 (run))
