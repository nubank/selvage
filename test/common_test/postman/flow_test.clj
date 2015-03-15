(ns common-test.postman.flow-test
  (:require [clojure.walk :as walk]
            [midje.sweet :refer :all]
            [common-test.postman.flow :as flow :refer [flow]]))


(defn tap [x]
  (println "<<<<<<")
  (clojure.pprint/pprint x)
  (println ">>>>>") x)

(def m macroexpand)
(def m1 macroexpand-1)
(def ma walk/macroexpand-all)


;(flow/execute-steps {} steps)

(defn step1 [world] (println "1") (assoc world :1 1))
(defn step2 [world] (println "2") (assoc world :2 2))
(defn step3 [world] (println "3") (assoc world :3 3))
(defn step4 [world] (println "4") (assoc world :4 4))
(defn step5 [world] (println "5") (assoc world :5 5))
(defn step6 [world] (println "6") (assoc world :6 6))


(let [s (new java.io.StringWriter)]
  (binding [*err* s]
    (fact 10 => 20)
    (str s)))

(clojure.pprint/with-pprint-dispatch clojure.pprint/code-dispatch
                                     (clojure.pprint/pprint (m `(fact 10 => 20))))

(m `(flow (fact 10 => 20)))

(flow (fact 10 => 20))


(flow step1
      step2
      (fact *world* => (embeds {:1 1 :2 2}))
      step3
      step4
      (fact *world* => (embeds {:3 3 :4 4}))
      step5
      step6)