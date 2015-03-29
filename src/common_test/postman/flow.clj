(ns common-test.postman.flow
  (:require [schema.macros :as sm]
            [schema.core :as s]
            [common-test.postman.core :refer [*world*]]
            [midje.emission.api :as m-emission])
  (:import (java.io StringWriter)))


(def expression s/Any)
(def step [(s/one (s/enum :transition :check) 'kind) (s/one [expression] 'expressions)])
(sm/defn forms->steps :- [step] [forms :- [expression]]
         (letfn [(form-check? [form] (and (coll? form) (-> form first name #{"fact" "facts"})))
                 (classify    [form] [(if (form-check? form) :check :transition) form])]
           (->> forms (map classify))))

(declare execute-steps)
(defmacro flow [& forms]
  (let [steps (forms->steps forms)]
    `(execute-steps {} ~steps)))


(defn emit [& strings]
  (when (m-emission/config-above? :print-nothing)
    (apply println strings)))

(defmacro execute-steps [world [step & rest]]
  (if step                                                ;TODO refactor to cond
    (let [[kind expr] step]
      (condp = kind
        :check
        `(let [writer#   (new StringWriter)
               new-world# ~world]
           (if (binding [*world* new-world#
                         clojure.test/*test-out* writer#]
                 ~expr)
             (execute-steps new-world# ~rest)
             (do (emit "Test failed with output:\n" (str writer#))
                 false)))
        :transition
        `(execute-steps (~expr ~world) ~rest)
        ))
    world))
