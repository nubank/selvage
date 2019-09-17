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

;; TODO: Do not fail if there's are no hooks
(defn discover-hooks []
  (-> "selvage.edn"
      io/resource
      slurp
      clojure.edn/read-string
      :hooks
      string->qualified-var
      (apply [])))

;; add tests
(defrecord NoOpFlowHooks [calls*]
  FlowHooks
  (setup [this flow-metadata])
  (before-step [this step-metadata world])
  (after-step [this step-metadata world])
  (teardown [this]))

(comment
  (discover-hooks)
  (def Hooks (TestFlowHooks. []))
  (.setup Hooks {:meta :foo}))
