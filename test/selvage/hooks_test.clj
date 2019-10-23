(ns selvage.hooks-test
  (:require [clojure.test :refer :all]
            [selvage.hooks :refer :all]))

(defrecord TestFlowHooks [calls*]
  FlowHooks
  (setup [this flow-metadata]
    (swap! calls* conj [:setup flow-metadata]))

  (before-step [this step-metadata world]
    (swap! calls* conj [:before-step step-metadata]))

  (after-step [this step-metadata world]
    (swap! calls* conj [:after-step step-metadata world]))

  (teardown [this]
    (swap! calls* reverse)))

(defn new-test-flow-hooks []
  (TestFlowHooks. (atom [])))

(deftest discover-hooks-test
  (is (instance? TestFlowHooks (discover-hooks))))
