(ns common-test.postman.flow
  (:require [schema.macros :as sm]
            [schema.core :as s]
            [common-test.postman.core :refer [*world*]]
            [midje.emission.api :as m-emission]
            [midje.emission.state :as m-state])
  (:import (java.io StringWriter)))

(defn check->fn [check-expr]
  (fn [world]
    (eval `(let [writer# (new StringWriter)]
             (m-state/with-isolated-output-counters
               [(binding [*world* ~world
                          clojure.test/*test-out* writer#]

                  ~check-expr) (str writer#)])))))

(def ^:dynamic *probe-timeout* 300)
(def ^:dynamic *probe-sleep-period* 10)

(def expression s/Any)
(def step [(s/one (s/enum :transition :check) 'kind) (s/one expression 'expression)])
;sm/defn forms->steps :- [step] [forms :- [expression]]
(sm/defn forms->steps :- [step] [forms :- [expression]]
  (letfn [(form-check? [form] (and (coll? form) (-> form first name #{"fact" "facts"})))
                 (classify    [form] (if (form-check? form) [:check form] [:transition form]))]
           (map classify forms)))

(declare execute-steps)

(defn emit [& strings]
  (when (m-emission/config-above? :print-nothing)
    (apply println strings)))

(defmacro time-run [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr
         elapsed# (/ (double (- (. System (nanoTime)) start#)) 1000000.0)]
     [elapsed# ret#]))

(defn retry? [elapsed-millis]
  (<= elapsed-millis *probe-timeout*))

(defn probe
  ([f] (probe f 0))
  ([f elapsed-so-far]
   (let [[time [test-passed? output]] (time-run (f))
         elapsed                    (+ elapsed-so-far time)]
     (cond test-passed?
           [true output]

           (retry? elapsed)
           (do (Thread/sleep *probe-sleep-period*)
               (probe f elapsed))

           :else
           [false output]))))

(defn gen-test-probe [world expr rest]
  `(let [[ok-or-fail# output#] (probe (partial (check->fn '~expr) ~world))]
     (if ok-or-fail#
       (execute-steps ~world ~rest)
       (do (emit "Test failed with output:\n" output#)
           false))))

(defn gen-transition [world expr rest]
  `(execute-steps (~expr ~world) ~rest))

(defmacro execute-steps [world [step & rest]]
  (if step
    (let [[kind expr] step]
      (condp = kind
        :check (gen-test-probe world expr rest)
        :transition (gen-transition world expr rest)))
    world))

(defmacro flow [& forms]
  (let [steps (forms->steps forms)]
    `(execute-steps {} ~steps)))
