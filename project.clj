(defproject status-term "0.1.0-SNAPSHOT"
  :description "A terminal dashboard for developers."
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [clojure-lanterna "0.9.7"]]
  :main ^:skip-aot status-term.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
