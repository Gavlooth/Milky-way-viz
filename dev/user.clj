(ns user
  (:require [clojure.pprint :refer [pprint]]
            [clojure.tools.namespace.repl :as tn]
            [mount.core :as mount]
            [milky-way.core :as core]))


(defn start [] (#'core/run))             ;; example on how to start app with certain states

(defn stop []
  (mount/stop))

(defn refresh []
  #_(stop)
  (tn/refresh))

(defn refresh-all []
  #_(stop)
  (tn/refresh-all))

(defn go
  "starts all states defined by defstate"
  []
  (start)
  :ready)

(defn reset
  "stops all states defined by defstate, reloads modified source files, and restarts the states"
  []
  (refresh-all)
  (refresh)
  (go))
