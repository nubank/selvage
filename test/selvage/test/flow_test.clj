(ns selvage.test.flow-test
  (:require [selvage.test.flow :as f :refer [*flow* *world* defflow defcheck]]
            [matcher-combinators.test]
            [clojure.test :as t :refer :all])
  (:import [java.io StringWriter]
           [clojure.lang ExceptionInfo]))

(defn- hide-test-from-runner!
  "rataria to make test runners not run `failing-flow`"
  [the-test-var]
  (alter-meta! the-test-var (fn [m] (-> m
                                        (assoc :flow-test (:test m))
                                        (dissoc :test)))))

(defn- capture-test-stats
  "runs test without affecting test-stats, but returns results of test run"
  [the-test-var]
  (let [output-counters-before @*report-counters*
        writer                 (new StringWriter)]
    (dosync (ref-set *report-counters* *initial-report-counters*))
    (let [var-meta         (meta the-test-var)
          updated-test-var (with-meta @the-test-var (assoc var-meta :test (:flow-test var-meta)))]
      (with-out-str
        (binding [*test-out* writer]
            (t/test-var updated-test-var))))
    (let [test-results @*report-counters*]
      (dosync (ref-set *report-counters* output-counters-before))
      test-results)))

(defcheck a-check
  (testing ":val is 0"
    (is (= 0 (:val *world*)))))

(deftest a-test
  (testing "a tautology"
    (is (= 1 1))))

(defn a-step [w]
  (assoc w :a-val 1))

(defflow failing-flow "shows how flows can work with clojure.test"
  (fn [w] (assoc w :val 0))

  a-check
  a-step
  a-test

  (testing (is (= 1 (:a-val *world*))))
  ;; example of failing test
  (testing (is (= 2 (:a-val *world*)))))
(hide-test-from-runner! #'failing-flow)

(deftest failing-flow-fails
  "Flow that has a failing test registers as a failure"
  (let [test-result (capture-test-stats (var failing-flow))]
    (is (= {:error 0 :fail 2 :pass 3 :test 3}
           test-result))))

(defflow error-flow
  (fn [w] (throw (ex-info "foo" {:bar 'baz}))))
(hide-test-from-runner! #'error-flow)

(deftest flow-with-bad-transition-fails-at-expand
  (is (thrown? ExceptionInfo (macroexpand '(defflow bad-transition 1)))))

(deftest error-flow-errors-out
  "Flow that errors out registers as error"
  (let [test-result (capture-test-stats (var error-flow))]
    (is (= {:error 1 :fail 1 :pass 0 :test 1}
           test-result))))

(defflow basic-flow "is 1 one?"
  (testing (is (= 1 1))))

(deftest verbose-behaviour
  "Turning on *verbose* results in each step being printed"
  (let [test-result (with-out-str
                      (binding [f/*verbose* false]
                        (basic-flow)))]
    (is (empty? test-result)))
  (is (not (empty?
             (with-out-str
               (binding [f/*verbose* true]
                 (basic-flow)))))))


(defflow query-retries
  "retyring query steps doesn't mess with test pass count"
  (fn [w] (assoc w :query-count (atom 0)))
  (testing (is (= @(:query-count *world*)
                  0)))
  (fn [w] w) ;; break the retry seq with a transistion

  (f/fnq [w] {:x (swap! (:query-count w) inc)})
  (testing (is (match? *world*
                       {:x 3}))))
