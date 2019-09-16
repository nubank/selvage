(ns selvage.hooks
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.java.classpath :as classpath]))

(defn- string->qualified-var
  "Turn a string representation of a namespace-qualified variable into a
        variable, loading the namespace in the process"
  [varstring]
  (let [varsymbol (symbol varstring)
        v         (symbol (name varsymbol))
        n         (symbol (namespace varsymbol))]
    (require n)
    (ns-resolve (find-ns n) v)))

(defprotocol FlowHooks
  (setup [this flow-metadata])
  (before-step [this step-metadata world])
  (after-step [this step-metadata world])
  (teardown [this]))

(defn discover-hooks []
  (-> "selvage.edn"
      io/resource
      slurp
      clojure.edn/read-string
      :hooks
      string->qualified-var
      (apply [])))

;; return no-op implementation of Hook
;; add tests

