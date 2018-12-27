(ns selvage.test.flow
  "clojure.test version of the flow macro"
   (:require [schema.core :as s]
             [selvage.core :as core]
             [clojure.spec.test.alpha :as spec.test]
             [clojure.test :as t]
             [selvage.visibility :as vis]
             [taoensso.timbre :as timbre])
  (:import [java.io ByteArrayOutputStream PrintStream StringWriter]))

(def ^:dynamic *probe-timeout* 300)
(def ^:dynamic *probe-sleep-period* 10)
(def ^:dynamic *verbose* false)
(def ^:dynamic *world* {})
(def ^:dynamic *flow* {})

(defn worlds [] (deref core/worlds-atom))

(def ^:dynamic *intermediate-report*)

(defn diff-test-result
  "Subtract two clojure.test style summary maps."
  [before after]
  {:pass  (apply - (map :pass [after before]))
   :error (apply - (map :error [after before]))
   :test  (apply - (map :test [after before]))
   :fail  (apply - (map :fail [after before]))})

(defn report-count []
  (diff-test-result *intermediate-report* @t/*report-counters*))

(defn update-report-counters [new-reports]
  (when t/*report-counters*
    (dosync (commute t/*report-counters*
                     (fn [current-counters] (merge-with + current-counters new-reports))))))

(defmacro with-report-counters
  [& body]
  `(update-report-counters
     (binding [*intermediate-report* (or (some-> t/*report-counters* deref) ~t/*initial-report-counters*)]
       (binding [t/*report-counters* (ref *intermediate-report*)]
         ~@body
         (diff-test-result *intermediate-report* @t/*report-counters*)))))

(defn- test-counter-reset
  "Grab current test stats and return clojure that resets to those states and
   runs function. Used for retrying function execution."
  [f]
  (let [output-counters-before @t/*report-counters*]
    (fn [& args]
      (dosync (ref-set t/*report-counters* output-counters-before))
      (apply f args))))

(defn retry [f]
  (letfn [(retry? [elapsed-millis] (<= elapsed-millis *probe-timeout*))
          (retry-f [elapsed-so-far f w]
            (let [[time [success? desc :as res]] (core/timed-apply f w)
                  elapsed                        (+ elapsed-so-far time)]
              (if success?
                res
                (if (retry? elapsed)
                  (do
                    (Thread/sleep *probe-sleep-period*)
                    ;; time accounting might be improved
                    (retry-f (+ elapsed *probe-sleep-period*) f w))
                  [false desc]))))]
    (partial retry-f 0
      (test-counter-reset f))))

(defn run-test-var [test-var]
  `(fn [] (t/test-var (var ~test-var))))

(defn run-test-expr [testing-expr]
  `(fn [] (do ~testing-expr)))

(defn report-diff->successful? [report-before report-after]
  (and (zero? (- (:fail report-before 0)
                 (:fail report-after 0)))
       (zero? (- (:error report-before 0)
                 (:error report-after 0)))))

(defn test->fn-expr [run-test-fn]
  `(fn [world#]
     (let [writer#         (new StringWriter)
           report-before# @t/*report-counters*]
       (binding [*world*                 world#
                 clojure.test/*test-out* writer#]
         (~run-test-fn)
         (let [result#  (report-diff->successful? report-before# @t/*report-counters*)
               success# (when result#
                          world#)]
           [success# (str writer#)])))))

(defn- is-test-var? [form]
  (and (symbol? form)
       (-> form resolve meta :test)))
(defn- is-testing? [form]
  (and (coll? form)
       (-> form first name (= "testing"))))
(defn- is-check-ref? [form]
  (and (coll? form)
       (-> form first name (= "testing"))))

(defn- classify [form]
  (cond
    (is-testing? form)         [:check
                                (-> form run-test-expr test->fn-expr)
                                (str form)]
    (is-test-var? form)        [:check
                                (-> form run-test-var test->fn-expr)
                                (str form)]
    (core/is-query? form)      [:query
                                (core/transition->fn-expr form)
                                (str form)]
    (core/is-transition? form) [:transition
                                (core/transition->fn-expr form)
                                (str form)]
    :else                      (throw (ex-info "unknown flow step type"
                                               {:form form}))))

(defmethod t/report ::flow [m]
  (t/with-test-out
    (case (:status m)
      :pass (t/inc-report-counter :pass)
      :fail (do (t/inc-report-counter :fail)
                (println "\n" (:flow m) "failed")))))

(defn announce-results [flow-description [success? desc]]
  (core/emit-debug-ln (str "Flow " flow-description " finished"
                             (if success?
                               " successfully"
                               " with failures") "\n")
                      {:flow-description flow-description
                       :log              :flow/finish
                       :success?         (boolean success?)})
  (let [report (if (boolean success?)
                 {:type ::flow :status :pass :flow flow-description}
                 {:type ::flow :status :fail :flow flow-description})]
    (t/do-report report))
  (when-not success?
    (core/stdout-emit desc)))

(defmacro with-cid [& body]
  `(vis/with-split-cid "FLOW"
     (let [result# (do ~@body)]
       result#)))

(defn get-flow-information
  [flow-name forms metadata]
  (let [[flow-title in-forms] (if (string? (first forms))
                                [(first forms) (rest forms)]
                                [nil forms])
        full-name             (str (ns-name *ns*) "/" flow-name ":" (:line metadata))
        flow-description      (if flow-title
                                (str full-name " \"" flow-title "\"")
                                full-name)]
    {:flow-description flow-description
     :flow-title       flow-title
     :in-forms         in-forms}))

(defmacro defflow
  "Define a flow test function that accepts no arguments.
  The body follows a world-transition system, where each expression is either a
  world-transition, a check, or a query. Checks and queries will be retried
  when checks fail."
  {:arglists '([name & body]
               [name description & body])}
  [name & forms]
  (let [{:keys [flow-title
                in-forms
                flow-description]} (get-flow-information name forms (meta &form))]
    `(do (~`t/deftest ~name
                      (spec.test/instrument)
                      (s/with-fn-validation
                        (binding [core/*report-fail-fn* #(t/inc-report-counter :error)
                                  core/*verbose*        *verbose*]
                          (with-cid
                            (core/announce-flow ~flow-description)
                            (with-report-counters
                              (->> (list ~@(core/forms->steps classify retry in-forms))
                                   core/run-steps
                                   (announce-results ~flow-description))))))
                      (spec.test/unstrument))
      (alter-meta! (var ~name) assoc :flow true))))

(defmacro ^::query fnq
  "Defines an anonymous retriable flow step. The flow will retry such steps
  adjacent checks fails."
  [& forms]
  `(fn ~@forms))

(defmacro defnq
  "Defines a retriable flow step. The flow will retry such steps adjacent
  checks fails."
  [name & forms]
  `(def ~(with-meta name {::query true}) (fn ~@forms)))

(defn register-flows-helper
  [filtered-flows]
  (let [filtered-flows-set (set filtered-flows)
        all-test-vars      (->> *ns* ns-publics vals)
        filter-fn          (or (and (seq filtered-flows)
                                    (fn [vr] (filtered-flows-set (-> vr meta :name))))
                               (comp :flow meta))
        sorted-flow-vars   (->> all-test-vars
                                (filter filter-fn)
                                (sort-by #(-> % meta :line)))]
    (fn [] (run! #(apply % nil) sorted-flow-vars))))

(defmacro register-flows
  "Finds all `defflow`s in the invoked namespace and registers them for
  `clojure.test` running via `test-ns-hook`.

  Optionally accepts `defflow` or `defftest` vars as arguments, and when
  provied, will only register those. This is a way to mix to test types
  registered."
  [& body]
  `(def ~'test-ns-hook ~(register-flows-helper body)))
