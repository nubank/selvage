(ns selvage.midje.flow-test
  (:require [matcher-combinators.midje :refer [match]]
            [matcher-combinators.matchers :as m]
            [selvage.core :as core]
            [selvage.midje.flow :as f :refer [*flow* *world* flow tabular-flow]]
            [midje.emission.api :as emission.api]
            [midje.emission.state :as emission.states]
            [midje.repl :refer [last-fact-checked]]
            [midje.sweet :refer :all]
            [taoensso.timbre :as timbre])
  (:import [clojure.lang Atom]))

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

(fact "flow has the CID used"
      (flow (fact "test" (+ 1 1) => 2)) => true

      (fact "flow meta contains the CID"
            (meta (last-fact-checked)) => (match {:selvage  true
                                                  :flow/cid #(re-find #"FLOW.*" %)})))

(fact "it exposes flow information"
      (flow
       (fact *flow* => (match {:name  #(re-find #"selvage.midje.flow-test\:\d+" %)
                               :title nil}))) => true

      (flow "title"
            (fact *flow* => (match {:name  #(re-find #"selvage.midje.flow-test\:\d+" %)
                                    :title "title"}))) => true)

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
            (fact *world* => (match (m/equals {:1 1 :2 2})))
            step3
            step4
            (fact *world* => (match (m/equals {:1 1 :2 2 :3 3 :4 4})))
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
         (fact *world* => (match {:atom #(instance? Atom %)}))
         #(assoc % :function (constantly 42))
         (fact *world* => (match {:function fn?}))
         #(assoc % :byte-array (byte-array 1))
         (fact *world* => (match {:byte-array anything})))
       => truthy)

(fact "flow fails when a step throws an exception"
      (emission.api/silently
        (timbre/with-level :fatal
          (flow step1
                (fn [_] (throw (ex-info "Some exception" {:a "a"})))
                step2)))
      => falsey
      (provided
        (step1 anything) => {}
        (step2 anything) => irrelevant :times 0))

(fact "flow should fail if 'transition' step doesn't return a valid world"
      (emission.api/silently
        (flow step1
              (fn [_] :not-a-valid-world)
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
  (emission.api/silently
   (def fact-when-step-succeeds
     (fact "this will succeed"
           (flow step1 (fact "passes" 1 => 1) step2) => truthy))

   (def fact-when-step-fails
     (fact "this will fail because a check fails"
           (flow step1 (fact "fails" 1 => 2) step2) => truthy))

   (def last-called (atom 0))
   (def stops-at-failure
     (fact "flow doesn't execute steps post failure"
           (flow (world-fn (reset! last-called 1))
                 (fact "nope" 1 => 2)
                 (world-fn (reset! last-called 2))) => truthy))

   (def step-throwing-exception-is-a-failure

     (fact "step throwing exception is also a test failure"
           (timbre/with-level :fatal
             (flow (fn [_] (throw (ex-info "expected exception" {:a "a"})))))
           => truthy))))

(facts "checking for success and failure"
  fact-when-step-succeeds => truthy
  fact-when-step-fails => falsey
  @last-called => 1
  step-throwing-exception-is-a-failure => falsey)

(facts "checks are retried"
  (let [counter (atom -1)]
    (def fails-first-run-then-succeeds
      (fact "this will succeed by retrying the fact (which increments the atom until it's pos?)"
            (flow (fact (swap! counter inc) => pos?)) => truthy))

   (facts "every check is retried until it passes"
     fails-first-run-then-succeeds => truthy)))

(def defnq-counts (atom {:step-1 0 :step-2 0 :step-3 0}))

(f/defnq query-step-1 [w]
       (swap! defnq-counts update-in [:step-1] inc))
(f/defnq query-step-3 [w]
       (swap! defnq-counts update-in [:step-3] inc))

(f/defnq factory-for-queries [key]
  (fn [world]
    (let [calls (swap! (:calls world) inc)]
      (if (> calls 2)
        (assoc world key ::finally-ok)
        world))))

(f/defnq query-taking-args [key world]
  (let [calls (swap! (:calls world) inc)]
    (if (> calls 2)
      (assoc world key ::finally-ok)
      world)))

(facts
  (let [query-count (atom 0)]
    (fact "query steps preceeding checks are also retried"
          (def succeeds-on-third-step-execution
            (fact
                  (flow (f/fnq [w]
                               {:x (swap! query-count inc)})
                        (fact *world* => (match {:x 3}))) => truthy))))

  (let [counts (atom {:step-1 0 :step-2 0 :step-3 0})]
    (fact "retries several query steps preceeding a check until it passes"
          (def preceeding-queries-succeed-on-third-step-execution
            (fact
                  (flow (f/fnq [w]
                               (swap! counts update-in [:step-1] inc))
                        (f/fnq [w]
                               (swap! counts update-in [:step-2] inc))
                        (f/fnq [w]
                               (swap! counts update-in [:step-3] inc))
                        (fact *world* => (match {:step-1 3 :step-2 3 :step-3 3}))) => truthy))))

  (fact "retries several query steps preceeding a check until it passes"
        (let [counts (atom {:non-query-step 0 :step-2 0 :step-3 0})]
          (fact "positive test"
                (def non-query-steps-are-not-retried-positive
                  (fact
                    (flow (fn [w]
                            (swap! counts update-in [:non-query-step] inc))
                          (f/fnq [w]
                                 (swap! counts update-in [:step-2] inc))
                          (f/fnq [w]
                                 (swap! counts update-in [:step-3] inc))
                          (fact *world* => (match {:step-2 3 :step-3 3}))) => truthy))))
        (let [counts (atom {:non-query-step 0 :step-2 0 :step-3 0})]
          (binding [f/*probe-timeout* 30 f/*probe-sleep-period* 1]
            (emission.api/silently
              (fact "negative test"
                    (def non-query-steps-are-not-retried-negative
                      (fact
                        (flow (fn [w]
                                (swap! counts update-in [:non-query-step] inc))
                              (f/fnq [w]
                                     (swap! counts update-in [:step-2] inc))
                              (f/fnq [w]
                                     (swap! counts update-in [:step-3] inc))
                              (fact *world* => (match {:non-query-step 3}))) => truthy)))))))

  (fact "only query steps immediately preceeding a check are retried"
        (let [counts (atom {:not-immediately-preceeding 0 :step-2 0 :step-3 0})]
          (fact "positive test"
                (def only-immediately-preceeding-query-steps-are-retried-positive
                  (fact
                    (flow (fn [w]
                            (swap! counts update-in [:not-immediately-preceeding] inc))
                          (f/fnq [w]
                                 (swap! counts update-in [:step-2] inc))
                          (f/fnq [w]
                                 (swap! counts update-in [:step-3] inc))
                          (fact *world* => (match {:step-2 3 :step-3 3}))) => truthy))))
        (let [counts (atom {:not-immediately-preceeding 0 :step-2 0 :step-3 0})]
          (binding [f/*probe-timeout* 10 f/*probe-sleep-period* 1]
            (emission.api/silently
              (fact "negative test, inserting a regular - perhaps imperative - step in-between query steps"
                    (def only-immediately-preceeding-query-steps-are-retried-negative
                      (fact
                        (flow (f/fnq [w]
                                (swap! counts update-in [:not-immediately-preceeding] inc))
                              step1
                              (f/fnq [w]
                                     (swap! counts update-in [:step-2] inc))
                              (f/fnq [w]
                                     (swap! counts update-in [:step-3] inc))
                              (fact *world* => (match {:not-immediately-preceeding 3}))) => truthy)))))))

  (fact "retries query steps marked via f/defnq"
        (def retries-with-defnq
          (fact
            (flow query-step-1
                  (f/fnq [w]
                         (swap! defnq-counts update-in [:step-2] inc))
                  query-step-3
                  (facts
                    (:step-1 *world*) => #(> % 3)
                    (:step-2 *world*) => #(> % 3)
                    (:step-3 *world*) => #(> % 3))) => truthy)))

  (fact "retries queries returned by factory functions"
        (def retries-factory-queries
          (fact
            (flow
             (fn [_] {:calls (atom 0)})

             (factory-for-queries :foo)

             (fact "fnq was retried 2 times until this test passed"
                   *world* => (match {:foo ::finally-ok}))) => truthy)))

  (fact "retries steps built by partially applying query functions"
        (def retries-partially-applied-queries
          (fact
            (flow
              (fn [_] {:calls (atom 0)})

              (partial query-taking-args :foo)

              (fact "fnq was retried 2 times until this test passed"
                    *world* => (match {:foo ::finally-ok}))) => truthy)))

  (fact "retries steps built by composing a query function with other functions"
        (def retries-comp-queries
          (fact
            (flow
              (fn [_] {:calls (atom 0)})

              (comp (partial query-taking-args :foo) identity)

              (fact "fnq was retried 2 times until this test passed"
                    *world* => (match {:foo ::finally-ok}))) => truthy)))


  (facts "checks and query steps are retried"
    succeeds-on-third-step-execution => truthy
    preceeding-queries-succeed-on-third-step-execution => truthy
    non-query-steps-are-not-retried-positive => truthy
    non-query-steps-are-not-retried-negative => falsey
    only-immediately-preceeding-query-steps-are-retried-positive => truthy
    only-immediately-preceeding-query-steps-are-retried-negative => falsey
    retries-with-defnq => truthy
    retries-factory-queries => truthy
    retries-partially-applied-queries => truthy
    retries-comp-queries => truthy))

(binding [f/*probe-timeout* 10
          f/*probe-sleep-period* 1]
  (facts "on the impact on a test run:"
    (fact "when a test passes, midje records no failures"
          (emission.api/silently
            (flow (fact true => truthy)) => truthy
            (emission.states/output-counters))
          => (match {:midje-failures 0
                     :midje-passes   1}))

    (fact "when a probe times out and fails, midje records that failure"
          (emission.api/silently
            (flow (fact false => truthy)) => falsey
            (emission.states/output-counters))
          => (match {:midje-failures 1}))

    (def counter2 (atom -2))
    (fact "when a test passes after a few tries, midje still records no failures"
          (emission.api/silently
            (flow (fact (swap! counter2 inc) => pos?)) => truthy
            (emission.states/output-counters))
          => (match {:midje-failures 0}))))

(facts "it logs ns and line number on flow"
       (fact "when a test description is given"
             (flow "test flow log" (fact 1 => 1)) => irrelevant
             (provided
               (core/emit-debug-ln #"Running flow: selvage.midje.flow-test:\d+ test flow log" anything) => irrelevant
               (core/emit-debug-ln anything anything) => irrelevant :times 3))

       (fact "when no test description is given"
             (flow (fact 1 => 1)) => irrelevant
             (provided
               (core/emit-debug-ln #"Running flow: selvage.midje.flow-test:\d+" anything) => irrelevant
               (core/emit-debug-ln anything anything) => irrelevant :times 3)))

(fact "wrap flow forms inside fact with metadata"
  (macroexpand-1 '(flow "rataria" (fact 1 => 1)))
  => (match
       (list 'schema.core/with-fn-validation
             (m/embeds
               (list 'midje.sweet/facts
                     :selvage
                     #(re-find #"selvage.midje.flow-test:[0-9]+ rataria" %))))))

(facts "Tabular works as expected"
       (emission.api/silently
         (tabular-flow
           (flow "Simple check"
                 (fact ?a => ?b))
           ?a ?b
           1 1
           2 2
           2 1)
         ;; All checks are doubled, because we need to wrap the flow in a fact.
         (emission.states/output-counters)) => {:midje-failures 2, :midje-passes 4})

(facts "future-fact"
  (let [future-check (atom [])]
    (fact "core flow with future-fact"
      (flow
        (fact "First valid assertion"
          (swap! future-check conj :first) => [:first])
        (future-fact "Second ignored assertion"
          (swap! future-check conj :second) => [:second])
        (fact "Third valid assertion"
          (swap! future-check conj :third) => [:first :third])) => true)
    (fact "check future-fact pass through"
      @future-check => [:first :third])))
