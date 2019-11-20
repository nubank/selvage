(ns selvage.midje.flow
  "midje version of the flow macro"
  (:require [selvage.core :as core]
            [selvage.visibility :as vis]
            [selvage.formatting :as formatting]
            [midje.emission.api :as emission.api]
            [midje.emission.state :as emission.state]
            [midje.repl :refer [last-fact-checked]]
            [midje.sweet :refer [fact facts tabular truthy]]
            [schema.core :as s]
            [taoensso.timbre :as timbre])
  (:import [clojure.lang ISeq Symbol]
           [java.io ByteArrayOutputStream PrintStream StringWriter]))

(def ^:dynamic *probe-timeout* 300)
(def ^:dynamic *probe-sleep-period* 10)
(def ^:dynamic *world* {})
(def ^:dynamic *flow* {})
(def ^:dynamic *verbose* false)
(def ^:dynamic *report-fail*)

(defn worlds [] (deref core/worlds-atom))

(defn- fact-desc [fact-form]
  (if (string? (second fact-form))
    (second fact-form)
    (str fact-form)))

(defn resetting-midje-counters [f]
  (let [output-counters-before (emission.state/output-counters)]
    (fn [& args]
      (emission.state/set-output-counters! output-counters-before)
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
    (partial retry-f 0 (resetting-midje-counters f))))

(defn check->fn-expr [check-expr]
  `(fn [world#]
     (let [writer# (new StringWriter)]
       (binding [*world*                 world#
                 clojure.test/*test-out* writer#]
         (let [result#  ~check-expr
               success# (when result#
                          world#)]
           [success# (str writer#)])))))

(defn future->fn-expr [form]
  `(fn [world#]
     ~form ; This shows 'WORK TO DO...' message on output
     [world# nil]))

(defn- is-check? [form]
  (and (coll? form)
       (-> form first name #{"fact" "facts"})))
(defn- is-future? [form]
  (and (coll? form)
       (-> form first name #{"future-fact" "future-facts"})))

(defn- classify [form]
  (cond (is-check? form)           [:check (check->fn-expr form) (fact-desc form)]
        (is-future? form)          [:check (future->fn-expr form) (fact-desc form)]
        (core/is-query? form)      [:query (core/transition->fn-expr form) (str form)]
        :else                      [:transition
                                    (core/transition->fn-expr form)
                                    (str form)]))

(defn announce-results [flow-description [success? desc]]
  (when-not success?
    (core/stdout-emit desc))
  (core/emit-debug-ln (str "Flow " flow-description " finished"
                             (if success?
                               " successfully"
                               " with failures") "\n") {:flow-description flow-description
                                                        :log              :flow/finish
                                                        :success?         (boolean success?)})
  (boolean success?))

(defn wrap-with-metadata [flow-name flow-expr]
  `(s/with-fn-validation
     (facts :selvage  ~flow-name
       ~flow-expr)))

(defn update-metadata-w-cid! []
  (-> (last-fact-checked)
      (vary-meta assoc :flow/cid (vis/current-cid))
      ;; HACK: re-record fact so the meta with CID is saved
      (midje.data.compendium/record-fact-check!)))

(defmacro with-cid [& body]
  `(vis/with-split-cid "FLOW"
     (let [result# (do ~@body)]
       (update-metadata-w-cid!)
       result#)))

(defn get-flow-information
  [forms metadata]
  (let [flow-ns               (ns-name *ns*)
        flow-name             (str flow-ns ":" (:line metadata))
        [flow-title in-forms] (if (string? (first forms))
                                [(first forms) (rest forms)]
                                [nil forms])
        flow-description      (if flow-title (str flow-name " " flow-title) flow-name)]
    {:flow-description flow-description
     :flow-ns          flow-ns
     :flow-name        flow-name
     :flow-title       flow-title
     :in-forms         in-forms}))

(defmacro flow
  "Defines a flow test.
  The body follows a world-transition system, where each expression is either a
  world-transition, a check, or a query. Checks and queries will be retried
  when checks fail."
  [& forms]
  (let [{:keys [flow-name
                flow-title
                in-forms
                flow-description]} (get-flow-information forms (meta &form))]
    (wrap-with-metadata flow-description
                        `(binding [*flow*                 {:name  ~flow-name
                                                           :title ~flow-title}
                                   core/*report-fail-fn* #(emission.state/output-counters:inc:midje-failures!)
                                   core/*verbose*        *verbose*
                                   core/*quiet*          (not (emission.api/config-above?
                                                                 :print-nothing))]
                           (with-cid
                             (core/announce-flow ~flow-description)
                             (->> (list ~@(core/forms->steps classify retry in-forms))
                                  (core/run-steps ~flow-name)
                                  (announce-results ~flow-description)))))))

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

(defmacro tabular-flow [flow & table]
  `(tabular
     (fact ~flow => truthy)
     ~@table))
