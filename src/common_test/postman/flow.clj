(ns common-test.postman.flow
  (:require #_[schema.macros :as sm]
            #_[schema.core :as s]
            [common-test.postman.core :refer [*world*]]
            [midje.emission.api :as m-emission]
            [midje.emission.state :as m-state])
  (:import (java.io StringWriter)))

(defn tap [x]
  (clojure.pprint/pprint x)
  x)


(defn check->fn [check-expr]
  (fn [world]

    (eval `(let [writer# (new StringWriter)]
             (m-state/with-isolated-output-counters
               [(binding [*world* ~world
                          clojure.test/*test-out* writer#]

                  ~check-expr) (str writer#)])))))

(def ^:dynamic *probe-timeout* 300)
(def ^:dynamic *probe-sleep-period* 10)

#_(def expression s/Any)
#_(def step [(s/one (s/enum :transition :check) 'kind) (s/one [expression] 'expressions)])
;sm/defn forms->steps :- [step] [forms :- [expression]]
(defn forms->steps  [forms]

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



#_(defmacro test-run
  ([world expr] `(test-run ~world expr 0))
  ([world expr elaspsed-so-far]
   `(let [writer# (new StringWriter)
          new-world# ~world]
      (let [[result# time#] (time-run (binding [*world* new-world#
                                                clojure.test/*test-out* writer#]
                                        ~expr))
            elapsed# (+ ~elaspsed-so-far time#)]
        (println "elapsed:: " elapsed#)
        (cond result#              [true (str writer#)]
              (retry? elapsed#)    (test-run ~world ~expr elapsed#)
              :else                [false (str writer#)])))))


(defn gen-test-probe [world expr rest]

  `(let [[ok-or-fail# output#] (probe (partial (check->fn '~expr) ~world))]
     (println "ok or fail::: " ok-or-fail#)
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
