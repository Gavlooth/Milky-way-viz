(ns user
  (:require [clojure.pprint :refer [pprint]]
            [clojure.tools.namespace.repl :as tn]
            [mount.core :as mount]
            #_[mount.tools.graph :refer [states-with-deps]]
            [milky-way.core :refer [run]]
            #_[milky-way.views :refer [draw]]))


(defn start [] (#'run))             ;; example on how to start app with certain states

(defn stop []
  (mount/stop))

(defn refresh []
  (stop)
  (tn/refresh))

(defn refresh-all []
  (stop)
  (tn/refresh-all))

(defn go
  "starts all states defined by defstate"
  []
  (start)
  :ready)

(defn reset
  "stops all states defined by defstate, reloads modified source files, and restarts the states"
  []
  (refresh)
  (go))
