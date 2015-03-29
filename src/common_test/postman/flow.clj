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
           (map classify forms)))

(declare execute-steps)
(defmacro flow [& forms]
  (let [steps (forms->steps forms)]
    `(execute-steps {} ~steps)))


(defn emit [& strings]
  (when (m-emission/config-above? :print-nothing)
    (apply println strings)))

(defn gen-test-run [world expr]
  `(let [writer#   (new StringWriter)
         new-world# ~world]
     [(binding [*world* new-world#
                clojure.test/*test-out* writer#]
        ~expr) (str writer#)]))

(defn gen-test-probe [world expr rest]
  `(let [[ok-or-fail# output#] ~(gen-test-run world expr)]
     (if ok-or-fail#
       (execute-steps ~world ~rest))))

(defn gen-transition [world expr rest]
  `(execute-steps (~expr ~world) ~rest))

(defmacro execute-steps [world [step & rest]]
  (if step
    (let [[kind expr] step]
      (condp = kind
        :check      (gen-test-probe world expr rest)
        :transition (gen-transition world expr rest)))
    world))
