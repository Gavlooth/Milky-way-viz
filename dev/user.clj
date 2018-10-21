(ns user
  (:require [clojure.pprint :refer [pprint]]
            [clojure.tools.namespace.repl :as tn]
            [mount.core :as mount]
            [milky-way.core :as core]))


(defn start [& args]
  (#'core/run (first args)))             ;; example on how to start app with certain states

(defn stop []
  (mount/stop))

(defn refresh []
  (tn/refresh))

(defn refresh-all []
  (tn/refresh-all))

(defn go
  "starts all states defined by defstate"
  [& args]
  (start (first args))
  :ready)

(defn reset
  "stops all states defined by defstate, reloads modified source files, and restarts the states"
  [& args]
  (refresh-all)
  (refresh)
  (go (first args)))
