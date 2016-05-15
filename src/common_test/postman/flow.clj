(ns common-test.postman.flow
  (:require [schema.core :as s]
            [midje.sweet :refer [fact facts anything]]
            [midje.emission.api :as m-emission]
            [midje.emission.state :as m-state]
            [common-core.visibility :as vis]
            [clojure.string :as str])
  (:import [java.io StringWriter]))

(def ^:dynamic *probe-timeout* 300)
(def ^:dynamic *probe-sleep-period* 10)
(def ^:dynamic *verbose* false)
(def ^:dynamic *world* {})

(def worlds-atom (atom {}))

(defn emit [& strings]
  (when (m-emission/config-above? :print-nothing)
    (apply print strings)))

(defn emit-ln [& strings]
  (emit (format "%-40s\t\t\t[CID: %s]\n" (str/join " " strings) (vis/current-cid))))

(defn emit-debug [& strings]
  (when *verbose* (apply emit strings)))

(defn emit-debug-ln [& strings]
  (emit-debug (format "%-40s\t\t\t[CID: %s]\n" (str/join " " strings) (vis/current-cid))))

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
(def step [(s/one (s/enum :transition :check :query) 'kind) (s/one expression 'expression)])

(defn- fact-desc [fact-form]
  (if (string? (second fact-form))
    (second fact-form)
    (str fact-form)))

(s/defn forms->steps :- [step] [forms :- [expression]]
  (letfn [(form-check? [form] (and (coll? form) (-> form first name #{"fact" "facts" "future-fact" "future-facts"})))
          (classify    [form] (if (form-check? form) [:check form (fact-desc form)] [:transition form (str form)]))]
    (map classify forms)))

(declare execute-steps)

(defn time-run [run-function]
  (let [start    (. System (nanoTime))
        ret     (run-function)
        elapsed (/ (double (- (. System (nanoTime)) start)) 1000000.0)]
     [elapsed ret]))

(defn retry? [elapsed-millis]
  (<= elapsed-millis *probe-timeout*))

(defn probe
  ([f] (probe f 0))
  ([f elapsed-so-far]
   (let [[time [test-passed? output]] (m-state/with-isolated-output-counters (time-run f))
         elapsed                      (+ elapsed-so-far time)]
     (cond test-passed?
           (do (m-emission/pass)
               (emit-debug-ln "test passed")
               [true output])

           (retry? elapsed)
           (do (Thread/sleep *probe-sleep-period*)
               (emit-debug "x")
               (probe f elapsed))

           :else
           (do
             (emit-debug "\n")
             (f))))))

(defn gen-test-probe [world expr rest]
  `(when (vis/with-split-cid
           (let [[ok-or-fail# output#] (do
                                         (emit-debug-ln "probing assertion...")
                                         (probe (partial (check->fn '~expr) ~world)))]
             (when-not ok-or-fail#
               (emit-ln "Test failed with output:\n" output# "\n"))
             ok-or-fail#))
     (execute-steps ~world ~rest)))

(defn gen-transition [world expr name rest]
  `(try
     (let [next-world# (vis/with-split-cid
                         (do
                           (emit-debug-ln (str "running " ~name))
                           (save-world-debug ~name (~expr ~world))))]
       (execute-steps next-world# ~rest))
     (catch Throwable throwable#
       (set! *e throwable#)                                 ; letting repl know of the exception
       (fact (throw throwable#) => (str "Step '" ~name "' to not throw an exception")) ; making the exception a test failure
       )))

(defmacro execute-steps [world [step & rest]]
  (if step
    (let [[kind expr name] step]
      (condp = kind
        :check (gen-test-probe world expr rest)
        :transition (gen-transition world expr name rest)))
    world))

(defn forms->flow [flow-name forms]
  (let [steps (forms->steps forms)]
    `(s/with-fn-validation
       (vis/with-split-cid "FLOW"
                           (facts :postman ~flow-name
                                 (do
                                   (emit-debug-ln (str "Running flow: " ~flow-name))
                                   (emit-debug-ln "Flow finished" (if (execute-steps {} ~steps)
                                                                    "succesfully"
                                                                    "with failures"))
                                   (emit-debug "\n")))))))

(defmacro flow-old [& forms]
  (let [flow-name (str (ns-name *ns*)
                       ":"
                       (:line (meta &form)))]
       (if (string? (first forms))
         (forms->flow (str flow-name " " (first forms)) (rest forms))
         (forms->flow flow-name forms))))

(defn check->fn-expr [check-expr]
  `(fn [world#]
     (let [writer# (new StringWriter)]
       (binding [*world* world#
                 clojure.test/*test-out* writer#]
         (emit-debug-ln "probing assertion...")             ;TODO: review output
         (let [result#  ~check-expr
               success# (when result#
                          (emit-debug-ln "test passed")
                          world#)]
           [success# (str writer#)])))))

; try-catch
(defn transition->fn-expr [transition-expr]
  `(fn [world#]
     (try [(~transition-expr world#) ""]
       (catch Throwable throwable#
         (fact (throw throwable#) => (str "Step '" ~(str transition-expr) "' to not throw an exception"))
         [false throwable#]))))

(defn retriable? [[kind f desc]]
  (-> kind #{:check :query} boolean))

(def max-retries 5)

(defn retry [f]
  (let [output-counters-before (m-state/output-counters)]
    (letfn [(retry-f [retries w]
              (m-state/set-output-counters! output-counters-before)
              (let [[success? _ :as res] (f w)]
                (if success?
                  res
                  (if (< retries max-retries)
                    (do
                      (Thread/sleep *probe-sleep-period*)
                      (retry-f (inc retries) w))
                    [false "timed out"]))))]
      (partial retry-f 0))))


(defn partition-group-by [pred coll]
  (->> coll (partition-by pred) (map #(vector (pred (first %)) %))))

(defn run-step [[world _] [step-type f desc]]
  (let [[next-world desc] (f world)]
    (if next-world
      [next-world desc]
      (reduced [next-world desc]))))

(defn run-step-sequence [s0 steps]
  (reduce run-step s0 steps))

(defn steps-to-step [steps]
  `[:sequence (fn [w#] (run-step-sequence [w# ""] (list ~@steps))) "multiple steps"])

(defn retry-expr [[kind f-expr desc]]
  `[:retry (fn [w#] ((retry ~f-expr) w#)) ~desc])

(defn retry-sequences [steps]
  (->> steps
       ;(map (fn [step] (if (retriable? step) (list retry step) step)))
       (partition-group-by retriable?)
       (mapcat (fn [[retriable-seq? steps]]
                 (if retriable-seq?
                   [(retry-expr (steps-to-step steps))]
                   steps)))
       ))

(s/defn forms->steps-exprs :- [step] [forms :- [expression]]
  (letfn [(is-check? [form] (and (coll? form) (-> form first name #{"fact" "facts" "future-fact" "future-facts"})))
          (is-query? [form] (-> form macroexpand meta ::query))
          (classify  [form] (cond (is-check? form) [:check      (check->fn-expr form) (fact-desc form)]
                                  (is-query? form) [:query      (transition->fn-expr form) (str form)]
                                  :else            [:transition (transition->fn-expr form) (str form)]))]
    (->> forms (map classify) retry-sequences seq)))

(defn run-steps [steps]
  (->> steps
       ;(map vector)
       ;(map steps-to-step)
       (run-step-sequence [{} ""])))

(defn format-result [flow-description [success? desc]]
  (do
    (emit-debug-ln (str "Running flow: " flow-description))
    (emit-debug-ln "Flow finished" (if success?
                                     "succesfully"
                                     "with failures"))
    (emit-debug "\n"))
  (boolean success?))

(defn wrap-with-metadata [flow-name flow-expr]
  `(s/with-fn-validation
     (vis/with-split-cid "FLOW"
                         (facts :postman ~flow-name
                           ~flow-expr))))

(defmacro flow [& forms]
  (let [flow-name (str (ns-name *ns*)
                       ":"
                       (:line (meta &form)))
        [flow-description in-forms] (if (string? (first forms))
                                      [(str flow-name " " (first forms)) (rest forms)]
                                      [flow-name forms])]
    (wrap-with-metadata flow-description
                        `(->> (list ~@(forms->steps-exprs in-forms))
                              run-steps
                              (format-result ~flow-description)))))
