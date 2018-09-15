(defproject milky-way "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [rm-hull/infix "0.3.3"]
                 [mount "0.1.11"]
                 [quil "2.6.0"]
                 [mount "0.1.12"]
                 [net.mikera/core.matrix "0.62.0"]
                 ;[org.clojure/tools.namespace "0.2.11"]
                 [org.apache.commons/commons-math3 "3.6.1"]]
  :profiles {:uberjar {:aot :all :uberjar-name "milky"}

             :dev {;:resource-paths ["test/dev-resources"]
                   :dependencies   [[venantius/pyro "0.1.2"]
                                    [org.clojure/tools.namespace "0.2.11"]]


                   :injections []

                   :plugins        [[lein-cljfmt "0.5.6"]
                                    [lein-kibit "0.1.5"]]
                   :source-paths ["dev"]
                   :repl-options {:init-ns user
                                  :init (do (set! *warn-on-reflection* true)
                                            (require '[pyro.printer :as printer])
                                            (printer/swap-stacktrace-engine!))}}})

