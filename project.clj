(defproject nubank/postman "0.0.1"
  :description "Macro for writing tests in world-transition pattern"
  :url "https://github.com/nubank/postman"
  :license {:name "Apache License, Version 2.0"}

  :repositories [["central" {:url "https://repo1.maven.org/maven2/" :snapshots false}]
                 ["clojars" {:url "https://clojars.org/repo/"}]]

  :plugins [[lein-midje "3.2.1"]
            [lein-ancient "0.6.14"]]

  :dependencies [[org.clojure/clojure "1.9.0"]
                 [io.aviso/pretty "0.1.34"]
                 [midje "1.9.2-alpha2" :exclusions [org.clojure/clojure]]
                 [com.taoensso/timbre "4.10.0"]]

  :profiles {:dev {:dependencies [[nubank/matcher-combinators "0.2.3"]]}
             :1.8 {:dependencies [[org.clojure/clojure "1.8.0"]]}})
