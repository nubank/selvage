(ns common-test.postman.flow
  (:require [schema.macros :as sm]
            [schema.core :as s]
            [midje.sweet :refer [fact anything]]
            [midje.emission.api :as m-emission]
            [midje.emission.state :as m-state])
  (:import (java.io StringWriter)))

(def ^:dynamic *probe-timeout* 300)
(def ^:dynamic *probe-sleep-period* 10)
(def ^:dynamic *verbose* false)
(def ^:dynamic *world* {})

(def worlds-atom (atom {}))

(defn emit [& strings]
  (when (m-emission/config-above? :print-nothing)
    (apply println strings)))

(defn emit-debug [& strings]
  (when *verbose* (apply emit strings)))

(defn save-world-debug [name world]
  (swap! worlds-atom assoc name world)
  world)

(defn worlds [] (deref worlds-atom))

(defn check->fn [check-expr]
  (fn [world]
    (let [writer (new StringWriter)]
      (binding [*world* world
                clojure.test/*test-out* writer]
        [(eval check-expr) (str writer)]))))

(def expression s/Any)
(def step [(s/one (s/enum :transition :check) 'kind) (s/one expression 'expression)])

(defn- fact-desc [fact-form]
  (if (string? (second fact-form))
    (second fact-form)
    (str fact-form)))

(sm/defn forms->steps :- [step] [forms :- [expression]]
  (letfn [(form-check? [form] (and (coll? form) (-> form first name #{"fact" "facts"})))
                 (classify    [form] (if (form-check? form) [:check form (fact-desc form)] [:transition form (str form)]))]
           (map classify forms)))

(declare execute-steps)

(defn time-run [run-function]
  (let [start (. System (nanoTime))
         ret (run-function)
         elapsed (/ (double (- (. System (nanoTime)) start)) 1000000.0)]
     [elapsed ret]))

(defn retry? [elapsed-millis]
  (<= elapsed-millis *probe-timeout*))

(defn probe
  ([f] (probe f 0))
  ([f elapsed-so-far]
   (let [[time [test-passed? output]] (m-state/with-isolated-output-counters (time-run f))
         elapsed                    (+ elapsed-so-far time)]
     (cond test-passed?
           (do (m-emission/pass)
               [true output])

           (retry? elapsed)
           (do (Thread/sleep *probe-sleep-period*)
               (when *verbose* (emit "\tprobe failed"))
               (probe f elapsed))

           :else
           (f)))))

(defn gen-test-probe [world expr rest]
  `(let [[ok-or-fail# output#] (probe (partial (check->fn '~expr) ~world))]
     (if ok-or-fail#
       (execute-steps ~world ~rest)
       (do (emit "Test failed with output:\n" output#)
           false))))

(defn gen-transition [world expr name rest]
  `(try
     (emit-debug (str "running " ~name))
     (let [next-world# (save-world-debug ~name
                                         (~expr ~world))]
       (execute-steps next-world# ~rest))
     (catch Throwable throwable#
       (set! *e throwable#)                  ; letting repl know of the exception
       (fact (throw throwable#) => (str "Step '" ~name "' to not throw an exception")) ; making the exception a test failure
       )))

(defmacro execute-steps [world [step & rest]]
  (if step
    (let [[kind expr name] step]
      (condp = kind
        :check (gen-test-probe world expr rest)
        :transition (gen-transition world expr name rest)))
    world))

(defmacro flow [& forms]
  (let [steps (forms->steps forms)]
    `(s/with-fn-validation
      (emit-debug "Running flow")
      (let [result# (execute-steps {} ~steps)]
        (emit-debug "Flow finished" (if result# "succesfully" "with failures"))
        result#))))
