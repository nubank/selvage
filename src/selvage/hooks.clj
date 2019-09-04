(ns selvage.hooks
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.java.classpath :as classpath]))

(defprotocol FlowHooks
  (setup [this flow-metadata])
  (before-step [this step-metadata world])
  (after-step [this step-metadata world])
  (teardown [this]))

(defn discover-hooks []
  )

(comment 
  (io/resource "selvage.edn")

  (Math/sqrt 1)

  (classpath/get-urls (ClassLoader/getSystemClassLoader)))
