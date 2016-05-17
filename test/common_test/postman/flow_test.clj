(ns common-test.postman.flow-test
  (:require [common-core.test-helpers :refer [embeds iso]]
            [midje.sweet :refer :all]
            [common-test.postman.flow :as f :refer [flow *world*]]
            [midje.emission.api :as m-emission]
            [midje.emission.state :as m-state])
  (:import (clojure.lang Atom)
           (java.io StringWriter)))

(defn step1 [world] (assoc world :1 1))
(defn step2 [world] (assoc world :2 2))
(defn step3 [world] (assoc world :3 3))
(defn step4 [world] (assoc world :4 4))
(defn step5 [world] (assoc world :5 5))
(defn step6 [world] (assoc world :6 6))

(fact "flow passes the world through transition functions"
      (flow) => true

      (flow step1) => true
      (provided (step1 {}) => {:1 1})

      (flow step1 step2) => true
      (provided (step1 {}) => {:1 1}
                (step2 {:1 1}) => {:1 1 :2 2})

      (flow "world goes through" step1 step2) => true
      (provided (step1 {}) => {:1 1}
                (step2 {:1 1}) => {:1 1 :2 2}))

(fact "embedding tests"
      (flow (fact 1 => 1)) => truthy)

(fact "flow interleaves world-transition functions and facts"
      (flow (fact 1 => 1) step1) => truthy

      (flow step1
            (fact *world* => {:1 1})) => truthy

      (flow step1
            (fact *world* => {:1 1})
            step2) => true
      (provided (step1 {}) => {:1 1}
                (step2 {:1 1}) => {:1 1 :2 2})

      (flow step1
            step2
            (fact *world* => (iso {:1 1 :2 2}))
            step3
            step4
            (fact *world* => (iso {:1 1 :2 2 :3 3 :4 4}))
            step5
            step6) => true
      (provided (step1 {}) => {:1 1}
                (step2 {:1 1}) => {:1 1 :2 2}
                (step3 {:1 1 :2 2}) => {:1 1 :2 2 :3 3}
                (step4 {:1 1 :2 2 :3 3}) => {:1 1 :2 2 :3 3 :4 4}
                (step5 {:1 1 :2 2 :3 3 :4 4}) => {:1 1 :2 2 :3 3 :4 4 :5 5}
                (step6 {:1 1 :2 2 :3 3 :4 4 :5 5}) => {:1 1 :2 2 :3 3 :4 4 :5 5 :6 6}))

(facts "handles non-homoiconic data"
       (flow
         #(assoc % :atom (atom 1))
         (fact *world* => (embeds {:atom #(instance? Atom %)}))
         #(assoc % :function (constantly 42))
         (fact *world* => (embeds {:function fn?}))
         #(assoc % :byte-array (byte-array 1))
         (fact *world* => (embeds {:byte-array anything})))
       => truthy)

(fact "flow fails when a step throws an exception"
      (m-emission/silently (flow
                             step1
                             (fn [_] (throw (ex-info "Some exception" {:a "a"})))
                             step2))
      => falsey
      (provided
        (step1 anything) => {}
        (step2 anything) => irrelevant :times 0))

(fact "flow accepts a string as the first form"
      (flow "2 + 2 = 4" (fact (+ 2 2) => 4)) => truthy)

(defmacro world-fn [& body]
  `(fn [world#] (do ~@body) world#))

(binding [f/*probe-timeout* 10]
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
                 (world-fn (reset! last-called 2))) => truthy))

   (def step-throwing-exception-is-a-failure
     (fact "step throwing exception is also a test failure"
           (flow (fn [_] (throw (ex-info "expected exception" {:a "a"}))))
           => truthy))))

(facts "checking for success and failure"
  fact-when-step-succeeds => truthy
  fact-when-step-fails => falsey
  @last-called => 1
  step-throwing-exception-is-a-failure => falsey)

(facts "checks are retried"
  (let [counter (atom -1)]
   (m-emission/silently
     (def fails-first-run-then-succeeds
       (fact "this will succeed by retrying the fact (which increments the atom until it's pos?)"
             (flow (fact (swap! counter inc) => pos?)) => truthy)))

   (facts "every check is retried until it passes"
     fails-first-run-then-succeeds => truthy)))


(binding [f/*probe-timeout* 10
          f/*probe-sleep-period* 1]
  (let [query-count (atom 0)]
   (fact "query steps preceeding checks are also retried"
         (def succeeds-on-third-step-execution
           (fact "test"
                 (flow (f/fnq [w]
                              {:x (swap! query-count inc)})
                       (fact *world* => (embeds {:x 3}))) => truthy)))

   (fact "retries one step preceeding a check until the check passes"
         succeeds-on-third-step-execution => truthy)))

(binding [f/*probe-timeout* 10
          f/*probe-sleep-period* 1]
  (facts "on the impact on a test run:"
    (fact "when a test passes, midje records no failures"
          (m-emission/silently
            (flow (fact true => truthy)) => truthy
            (m-state/output-counters))
          => (embeds {:midje-failures 0
                      :midje-passes   1}))

    (fact "when a probe times out and fails, midje records that failure"
          (m-emission/silently
            (flow (fact false => truthy)) => falsey
            (m-state/output-counters))
          => (embeds {:midje-failures 1}))

    (def counter2 (atom -2))
    (fact "when a test passes after a few tries, midje still records no failures"
          (m-emission/silently
            (flow (fact (swap! counter2 inc) => pos?)) => truthy
            (m-state/output-counters))
          => (embeds {:midje-failures 0}))))

(facts "it logs ns and line number on flow"
       (fact "when a test description is given"
             (flow "test flow log" (fact 1 => 1)) => irrelevant
             (provided
               (f/emit-debug-ln #"Running flow: common-test.postman.flow-test:\d+ test flow log") => irrelevant
               (f/emit-debug-ln anything & anything) => irrelevant :times 3))

       (fact "when no test description is given"
             (flow (fact 1 => 1)) => irrelevant
             (provided
               (f/emit-debug-ln #"Running flow: common-test.postman.flow-test:\d+") => irrelevant
               (f/emit-debug-ln anything & anything) => irrelevant :times 3)))

(fact "wrap flow forms inside fact with metadata"
      (macroexpand-1 '(flow "rataria" (fact 1 => 1)))
      =>
      (embeds
        '(schema.core/with-fn-validation
          (common-core.visibility/with-split-cid "FLOW"
                                                 (midje.sweet/facts :postman #"common-test.postman.flow-test:[0-9]+ rataria"
                                                   )))))
