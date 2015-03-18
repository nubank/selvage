(ns common-test.postman.flow
  (:require [schema.macros :as sm]
            [schema.core :as s]
            [common-test.postman.core :refer [*world*]]
            [midje.emission.api :as m-emission]))


(def expression s/Any)
(def step [(s/one (s/enum :transition :check) 'kind) (s/one [expression] 'expressions)])
(sm/defn forms->steps :- [step] [forms :- [expression]]
         (letfn [(form-check? [form] (and (coll? form) (-> form first name #{"fact" "facts"})))
                 (classify    [partition] [(if (form-check? (first partition)) :check :transition) partition])]
           (->> forms (partition-by form-check?) (map classify))))

(declare execute-steps)
(defmacro flow [& forms]
  (let [steps (forms->steps forms)]
    `(execute-steps {} ~steps)))


(defn emit [& strings]
  (when (m-emission/config-above? :print-nothing)
    (apply println strings)))

(defmacro execute-steps [world [step & rest]]
  ;(println "world " world " step " step)
  (if step                                                ;TODO refactor to cond
    (let [[kind exprs] step]
      (if (= :check kind)
        `(let [writer#   (new java.io.StringWriter)
               new-world# ~world]
           (if (binding [*world* new-world#
                         clojure.test/*test-out* writer#]
                     (and ~@exprs))
             (execute-steps new-world# ~rest)
             (do (emit "Test failed with output:\n" (str writer#))
                 false)))
        `(execute-steps ~(reduce #(cons %2 (list %1)) world exprs) ~rest)
        ))
    world))
