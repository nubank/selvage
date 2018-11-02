(defproject nubank/selvage "0.0.1"
  :description "Macro for writing tests in world-transition pattern"
  :url "https://github.com/nubank/selvage"
  :license {:name "Apache License, Version 2.0"}

  :repositories [["central" {:url "https://repo1.maven.org/maven2/" :snapshots false}]
                 ["clojars" {:url "https://clojars.org/repo/"}]]

  :plugins [[lein-midje "3.2.1"]
            [lein-ancient "0.6.15"]]

  :dependencies [[org.clojure/clojure "1.9.0"]
                 [io.aviso/pretty "0.1.35"]
                 [com.fasterxml.jackson.core/jackson-databind "2.9.6"]
                 [com.fasterxml.jackson.dataformat/jackson-dataformat-cbor "2.9.6"]
                 [midje "1.9.4" :exclusions [org.clojure/clojure]]
                 [prismatic/schema "1.1.9"]
                 [com.google.guava/guava "23.0"]
                 [com.taoensso/timbre "4.10.0"]]

  :profiles {:dev {:dependencies [[nubank/matcher-combinators "0.4.2"]]}
             :1.8 {:dependencies [[org.clojure/clojure "1.8.0"]]}})
