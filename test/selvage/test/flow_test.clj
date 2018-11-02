(ns selvage.test.flow-test
  (:require [selvage.test.flow :as f :refer [*flow* *world* defflow]]
            [matcher-combinators.test]
            [clojure.test :refer :all]))

(deftest a-test
  (println "running a-test")
  (testing ":val is 0"
    (is (= 0 (:val *world*)))))

(deftest b
  (testing "foo"
    (is (= 1 1))))
(deftest c
  (testing "foo"
    (is (= 2 1))))

(deftest b-test
  (println "running b test")
  (testing ":val is 1"
    (is (= 1 (:val *world*)))))

(defn a-step [w]
  (assoc w :a-val 1))

(defflow flow "shows how flows can work with clojure.test"
  (fn [w] (assoc w :val 0))

  a-test

  a-step

  (testing (is (= 1 (:a-val *world*))))
  ;; example of failing test
  (testing (is (= 2 (:a-val *world*)))))

;; you run the defined flow via:
;; (flow)


(def query-count (atom 0))
(defflow query-retries
  "retyring query steps doesn't mess with test pass count"
  (testing (is (= @query-count
                  0)))
  (fn [w] ;; break the retry seq with a transistion
    w)

  (f/fnq [w] {:x (swap! query-count inc)})
  (testing (is (match? *world*
                       {:x 3}))))

;; you can set the test runner to only run `defflow`s and not `deftest`s
;; ideally this will be done more or less automatically
(defn test-ns-hook []
  (flow)
  (query-retries))

(run-tests)
