(defproject pmbauer.blog/text "1.0.0"
  :description "text routines for article"
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.5.1"]]
  :profiles {:dev {:dependencies [[org.clojure/tools.namespace "0.2.3"]
                                  [org.clojure/data.generators "0.1.2"]
                                  [criterium "0.4.2"]]
                   :source-paths ["dev"]}}
  :jvm-opts ^:replace ["-XX:-UseConcMarkSweepGC"])
