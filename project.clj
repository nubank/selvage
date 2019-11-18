(defproject nubank/selvage "1.0.0-hooks"
  :description "Macro for writing tests in world-transition pattern"
  :url "https://github.com/nubank/selvage"
  :license {:name "Apache License, Version 2.0"}

  :repositories [["central" {:url "https://repo1.maven.org/maven2/" :snapshots false}]
                 ["clojars" {:url "https://clojars.org/repo/"}]]

  :plugins [[lein-midje "3.2.1"]
            [lein-vanity "0.2.0"]
            [lein-cljfmt "0.5.7"]
            [lein-kibit "0.1.6"]
            [lein-ancient "0.6.15"]]

  :cljfmt {:indents {flow               [[:block 1]]
                     facts              [[:block 1]]
                     fact               [[:block 1]]
                     as-customer        [[:block 1]]
                     as-of              [[:block 1]]
                     assoc-if           [[:block 1]]
                     let-entities       [[:block 2]]
                     provided           [[:inner 0]]
                     tabular            [[:inner 0]]
                     consume!           [[:block 0]]
                     try-type           [[:block 0]]
                     with-fn-validation [[:block 0]]
                     system-map         [[:block 0]]
                     parse-schema       [[:inner 0]]}}


  :dependencies [[org.clojure/clojure "1.9.0"]
                 [io.aviso/pretty "0.1.35"]
                 [com.fasterxml.jackson.core/jackson-databind "2.9.6"]
                 [com.fasterxml.jackson.dataformat/jackson-dataformat-cbor "2.9.6"]
                 [midje "1.9.4" :exclusions [org.clojure/clojure]]
                 [prismatic/schema "1.1.9"]
                 [com.google.guava/guava "23.0"]
                 [com.taoensso/timbre "4.10.0"]]

  :profiles {:dev    {:resource-paths ["test-resources/"]
                      :dependencies [[nubank/matcher-combinators "0.4.2"]
                                     [eftest "0.5.4"]]}
             :kaocha {:dependencies [[lambdaisland/kaocha "0.0-313"]]}
             :1.8    {:dependencies [[org.clojure/clojure "1.8.0"]]}}

  :aliases {"run-dev"         ["with-profile" "+repl-start" "trampoline" "repl" ":headless"]
            "run-dev-notramp" ["with-profile" "+repl-start" "repl" ":headless"]
            "lint"            ["do" "cljfmt" "check," "kibit"]
            "lint-fix"        ["do" "cljfmt" "fix," "kibit" "--replace"]
            "kaocha"          ["with-profile" "+kaocha" "run" "-m" "kaocha.runner"]})
