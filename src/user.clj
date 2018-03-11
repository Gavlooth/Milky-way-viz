(ns  user
  (:require [clojure.pprint :refer [pprint]]
            [clojure.tools.namespace.repl :as tn]
            [mount.core :as mount :refer [defstate]]
            [milky-way.views :as views]
            [mount.tools.graph :refer [states-with-deps]]
            [milky-way.core :refer [run]]))

(set! *warn-on-reflection* true)


(defn start []
  (mount/start
    #'milky-way.core/run))             ;; example on how to start app with certain states

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
  [& {:keys [draw]}]
  (start)
  ;;#_(run :draw draw) :ready

  )

(defn reset
  "stops all states defined by defstate, reloads modified source files, and restarts the states"
  []
  (stop)
  (tn/refresh :after 'user/go))

(mount/in-clj-mode)
