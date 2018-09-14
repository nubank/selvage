(ns selvage.cflow-test
  (:require [selvage.cflow :as f :refer [*flow* *world* defflow]]
            [clojure.test :refer :all]))

(deftest a-test
  (testing ":val is 0"
    (is (= 0 (:val *world*)))))

(deftest b-test
  (testing ":val is 1"
    (is (= 1 (:val *world*)))))

(defn a-step [w]
  (assoc w :a-val 1))

(defflow flow "shows how flows can work with clojure.test"
  (fn [w] (assoc w :val 0))

  (fn [w] (println "BEFORE a-test") w)
  a-test
  (fn [w] (println "AFTER a-test") w)

  a-step

  (fn [w] (println "BEFORE testing") w)
  (testing (is (= 1 (:a-val *world*))))
  (fn [w] (println "AFTER testing") w))

;; you run the defined flow via:
;; (flow)

;; you can set the test runner to only run `defflow`s and not `deftest`s
;; ideally this will be done more or less automatically
(defn test-ns-hook []
  (println "START test hook")
  (flow)
  (println "END test hook"))

(run-tests)

(comment
(clojure.pprint/pprint
  (macroexpand
    `(defflow flow
       (fn [w] (assoc w :val 0))
       a-test)))
)
