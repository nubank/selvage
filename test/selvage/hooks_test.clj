(ns selvage.hooks-test
  (:require [selvage.hooks :refer :all]
            [clojure.test :refer :all]))

(defrecord TestFlowHooks [calls*]
  FlowHooks
  (setup [this flow-metadata]
    (swap! calls* conj [:setup flow-metadata]))

  (before-step [this step-metadata world]
    (swap! calls* conj [:before-step step-metadata world]))

  (after-step [this step-metadata world]
    (swap! calls* conj [:after-step step-metadata world]))

  (teardown [this]
    (swap! calls* reverse)))

(defn new-test-flow-hooks[]
  (TestFlowHooks. nil))

(deftest discover-hooks-test
  (is (instance? TestFlowHooks (discover-hooks))))
