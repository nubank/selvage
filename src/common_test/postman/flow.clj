(ns common-test.postman.flow
  (:require [schema.core :as s]
            [midje.sweet :refer [fact facts anything tabular truthy]]
            [midje.repl :refer [last-fact-checked]]
            [midje.emission.api :as m-emission]
            [midje.emission.state :as m-state]
            [common-core.visibility :as vis]
            [clojure.string :as str])
  (:import [java.io StringWriter ByteArrayOutputStream PrintStream]
           [clojure.lang Symbol PersistentList IPersistentList ISeq]))

(def ^:dynamic *probe-timeout* 300)
(def ^:dynamic *probe-sleep-period* 10)
(def ^:dynamic *verbose* false)
(def ^:dynamic *world* {})

(def worlds-atom (atom {}))

(defn emit [& strings]
  (when (m-emission/config-above? :print-nothing)
    (apply print strings)
    (flush)))

(defn emit-ln [& strings]
  (emit (format "%-70s\t\t\t[CID: %s]\n" (str/join " " strings) (vis/current-cid))))

(defn emit-debug [& strings]
  (when *verbose* (apply emit strings)))

(defn emit-debug-ln [& strings]
  (emit-debug (format "%-70s\t\t\t[CID: %s]\n" (str/join " " strings) (vis/current-cid))))

(defn save-world-debug! [name world]
  (swap! worlds-atom assoc name world)
  world)

(defn worlds [] (deref worlds-atom))

(def Expression s/Any)
(def Step [(s/one (s/enum :transition :check :query) 'kind) (s/one Expression 'Expression)])

(defn- fact-desc [fact-form]
  (if (string? (second fact-form))
    (second fact-form)
    (str fact-form)))

(defn retriable-step? [[kind _f _desc]]
  (-> kind #{:check :query} boolean))

(defn resetting-midje-counters [f]
  (let [output-counters-before (m-state/output-counters)]
    (fn [& args]
      (m-state/set-output-counters! output-counters-before)
      (apply f args))))

(defn timed-apply [run-function & args]
  (let [start (System/nanoTime)
        ret (apply run-function args)
        elapsed (/ (double (- (System/nanoTime) start)) 1000000.0)]
    [elapsed ret]))

(defn run-step [[world _] [step-type f desc]]
  (vis/with-split-cid
    (do
      (emit-debug-ln "Running " (format "%-10s" (name step-type)) " " desc)
      (let [[next-world result-desc] (f world)]
        (save-world-debug! desc next-world)
        (if next-world
          [next-world result-desc]
          (reduced [next-world result-desc]))))))

(defn run-step-sequence [s0 steps]
  (reduce run-step s0 steps))

(defn run-steps [steps]
  (reset! worlds-atom {})
  (run-step-sequence [{} ""] steps))

(defn steps-to-step [steps]
  `[:sequence (fn [w#] (run-step-sequence [w# ""] (list ~@steps))) "running multiple steps"])

(defn retry [f]
  (letfn [(retry? [elapsed-millis] (<= elapsed-millis *probe-timeout*))
          (retry-f [elapsed-so-far f w]
            (let [[time [success? desc :as res]] (timed-apply f w)
                  elapsed (+ elapsed-so-far time)]
              (if success?
                res
                (if (retry? elapsed)
                  (do
                    (Thread/sleep *probe-sleep-period*)
                    (retry-f (+ elapsed *probe-sleep-period*) f w)) ; time accounting might be improved
                  [false desc]))))]
    (partial retry-f 0 (resetting-midje-counters f))))

(defn retry-expr [[_kind f-expr desc]]
  `[:retry (fn [w#] ((retry ~f-expr) w#)) ~desc])

(defn- partition-group-by [pred coll]
  (->> coll (partition-by pred) (map #(vector (pred (first %)) %))))

(defn retry-sequences [steps]
  (->> steps
       (partition-group-by retriable-step?)
       (mapcat (fn [[retriable-seq? steps]]
                 (if retriable-seq?
                   [(retry-expr (steps-to-step steps))]
                   steps)))))

(defn check->fn-expr [check-expr]
  `(fn [world#]
     (let [writer# (new StringWriter)]
       (binding [*world* world#
                 clojure.test/*test-out* writer#]
         (let [result# ~check-expr
               success# (when result#
                          world#)]
           [success# (str writer#)])))))

(defn print-exception-string [exception]
  (let [output-baos (ByteArrayOutputStream.)]
    (.printStackTrace exception (PrintStream. output-baos))
    (String. (.toByteArray output-baos) "UTF-8")))

(defn fail [expr-str & failure-messages]
  (m-state/output-counters:inc:midje-failures!)
  [false (apply str "Step '" expr-str "' " failure-messages)])

(defn valid-world-result [world expr-str]
  (if (map? world)
    [world ""]
    (fail expr-str "did not result in a map (i.e. a valid world):\n'" world "'")))

(defn transition->fn-expr [transition-expr]
  `(fn [world#]
     (try
       (valid-world-result (~transition-expr world#) ~(str transition-expr))
       (catch Throwable throwable#
         (fail ~(str transition-expr) "threw exception:\n" (print-exception-string throwable#))))))

(defmulti form->var class)

(defmethod form->var Symbol [s]
  (resolve s))

(defmethod form->var ISeq [l]
  (if (symbol? (first l)) (form->var (first l)) nil))

(s/defn forms->steps :- [Step] [forms :- [Expression]]
  (letfn [(is-check? [form] (and (coll? form) (-> form first name #{"fact" "facts" "future-fact" "future-facts"})))
          (is-query? [form] (-> form form->var meta ::query))
          (classify [form] (cond (is-check? form) [:check (check->fn-expr form) (fact-desc form)]
                                 (is-query? form) [:query (transition->fn-expr form) (str form)]
                                 :else [:transition (transition->fn-expr form) (str form)]))]
    (->> forms (map classify) retry-sequences seq)))

(defn announce-flow [flow-description]
  (emit-debug-ln (str "Running flow: " flow-description)))

(defn announce-results [flow-description [success? desc]]
  (when-not success?
    (emit-ln desc))
  (emit-debug-ln "Flow " flow-description " finished"
                 (if success?
                   "succesfully"
                   "with failures") "\n")
  (boolean success?))

(defn wrap-with-metadata [flow-name flow-expr]
  `(s/with-fn-validation
     (facts :postman ~flow-name
       ~flow-expr)))

(defn update-metadata-w-cid! []
  (-> (last-fact-checked)
      (vary-meta assoc :flow/cid vis/*cid*)
      (midje.data.compendium/record-fact-check!))) ;; HACK: re-record fact so the meta with CID is saved

(defmacro with-cid [& body]
  `(vis/with-split-cid "FLOW"
                       (let [result# (do ~@body)]
                         (update-metadata-w-cid!)
                         result#)))

(defmacro flow [& forms]
  (let [flow-name (str (ns-name *ns*) ":" (:line (meta &form)))
        [flow-description in-forms] (if (string? (first forms))
                                      [(str flow-name " " (first forms)) (rest forms)]
                                      [flow-name forms])]
    (wrap-with-metadata flow-description
                        `(with-cid
                           (announce-flow ~flow-description)
                           (->> (list ~@(forms->steps in-forms))
                                run-steps
                                (announce-results ~flow-description))))))

(defmacro ^::query fnq [& forms]
  `(fn ~@forms))

(defmacro defnq [name & forms]
  `(def ~(with-meta name {::query true}) (fn ~@forms)))

(defmacro tabular-flow [flow & table]
  `(tabular
     (fact ~flow => truthy)
     ~@table))
