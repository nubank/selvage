(ns common-test.postman.flow-test
  (:import [java.util ArrayList]
           (java.time LocalDateTime))
  (:require [clojure.walk :as walk]
            [common-core.test-helpers :refer [embeds iso]]
            [midje.sweet :refer :all]
            [midje.config :as midje-config]
            [midje.emission.state :as midje-state]
            [common-test.postman.flow :as flow :refer [flow]]
            [common-test.postman.core :refer [*world*]]
            [midje.emission.api :as m-emission]))

(defn tap [x]
  (clojure.pprint/pprint x))

(def m macroexpand)
(def m1 macroexpand-1)
(def ma walk/macroexpand-all)

(defn step1 [world] (assoc world :1 1))
(defn step2 [world] (assoc world :2 2))
(defn step3 [world] (assoc world :3 3))
(defn step4 [world] (assoc world :4 4))
(defn step5 [world] (assoc world :5 5))
(defn step6 [world] (assoc world :6 6))

(fact "flow passes the world through transition functions"
  (flow step1 step2) => (iso {:1 1 :2 2}))

(fact "flows runs a test"
      (flow (fact 1 => 1)) => truthy)

(fact "flow interleaves world-transition functions and facts"
      (flow step1
            step2
            (fact *world* => (iso {:1 1 :2 2}))
            step3
            step4
            (fact *world* => (iso {:1 1 :2 2 :3 3 :4 4}))
            step5
            step6) => (iso {:1 1 :2 2 :3 3 :4 4 :5 5 :6 6}))

(defmacro world-fn [& body]
  `(fn [world#] (do ~@body) world#))

; This will print a test failure, but
; will not affect the result of `lein midje`
(m-emission/silently
  (def fact-when-step-succeeds
    (fact "this will succeed"
          (flow step1 (fact "passes" 1 => 1) step2) => truthy))

  (def fact-when-step-fails
    (fact "this will fail because a check fails (failure output is normal)"
          (flow step1 (fact "fails" 1 => 2) step2) => truthy))

  (def last-called (atom 0))
  (def stops-at-failure
    (fact "flow doesn't execute steps post failure"
          (flow (world-fn (reset! last-called 1))
                (fact "nope" 1 => 2)
                (world-fn (reset! last-called 2))) => truthy)))

(facts "checking for success and failure"
       fact-when-step-succeeds => truthy
       fact-when-step-fails => falsey
       @last-called => 1)
