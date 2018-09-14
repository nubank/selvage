(ns selvage.cflow
   (:require [schema.core :as s]
             [clojure.test :as t]
             [selvage.visibility :as vis]
             [taoensso.timbre :as timbre])
  (:import [clojure.lang ISeq Symbol]
           [java.io ByteArrayOutputStream PrintStream StringWriter]))

(def ^:dynamic *probe-timeout* 300)
(def ^:dynamic *probe-sleep-period* 10)
(def ^:dynamic *world* {})
(def ^:dynamic *flow* {})
(def ^:dynamic *verbose* false)

(def worlds-atom (atom {}))

(defn emit-ln [message log-map]
  (timbre/info :log-map (assoc log-map :cid (vis/current-cid))))

(defn emit-debug-ln [message log-map]
  (when *verbose*
    (emit-ln message log-map)))

(defn save-world-debug! [name world]
  (swap! worlds-atom assoc name world)
  world)

(defn worlds [] (deref worlds-atom))

(def Expression s/Any)
(def Step [(s/one (s/enum :transition :retry) 'kind)
           (s/one Expression 'expression)
           (s/one s/Str 'description)])

(defn run-step [[world _] [step-type f desc]]
  (vis/with-split-cid
    (do
      (emit-debug-ln (str "Running " (format "%-10s" (name step-type)) " " desc)
                     {:log       :flow/run-step
                      :step-type step-type
                      :step-desc desc})
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

(defn retriable-step? [[kind _f _desc]]
  (-> kind #{:check :query} boolean))

(defn- partition-group-by [pred coll]
  (->> coll (partition-by pred) (map #(vector (pred (first %)) %))))

(defn timed-apply [run-function & args]
  (let [start   (System/nanoTime)
        ret     (apply run-function args)
        elapsed (/ (double (- (System/nanoTime) start)) 1000000.0)]
    [elapsed ret]))

(defn resetting-midje-counters [f]
  ;; TODO
  (let [output-counters-before nil ;@t/*report-counters*
        ]
    (fn [& args]
      ;;(emission.state/set-output-counters! output-counters-before)
      (apply f args))))

(defn retry [f]
  (letfn [(retry? [elapsed-millis] (<= elapsed-millis *probe-timeout*))
          (retry-f [elapsed-so-far f w]
            (let [[time [success? desc :as res]] (timed-apply f w)
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
      (resetting-midje-counters f))))


(defn retry-expr [[_kind f-expr desc]]
  `[:retry (fn [w#] ((retry ~f-expr) w#)) ~desc])

(defn retry-sequences [steps]
  (->> steps
       (partition-group-by retriable-step?)
       (mapcat (fn [[retriable-seq? steps]]
                 (if retriable-seq?
                   [(retry-expr (steps-to-step steps))]
                   steps)))))

(defn test-var->fn-expr [test-var]
  `(fn [world#]
     (let [writer# (new StringWriter)]
       (binding [*world*                 world#
                 clojure.test/*test-out* writer#
                 t/*report-counters*     (ref t/*initial-report-counters*)]
         (t/test-var (var ~test-var))
         (let [result#  (not (or (:fail @t/*report-counters*)
                                 (:error @t/*report-counters*)))
               success# (when result#
                          world#)]
           [success# (str writer#)])))))

(defn testing->fn-expr [testing-expr]
  `(fn [world#]
     (let [writer# (new StringWriter)]
       (binding [*world*                 world#
                 clojure.test/*test-out* writer#]
         (let [result#  ~testing-expr
               success# (when result#
                          world#)]
           [success# (str writer#)])))))

(defn fail [expr-str details & failure-messages]
  ;; TODO 
  (t/inc-report-counter :fail)
  [false (apply str "\033[0;33m  Step " expr-str " " details " \033[0m " failure-messages)])

(defn valid-world-result [world expr-str]
  (if (map? world)
    [world ""]
    (fail expr-str "did not result in a map (i.e. a valid world):\n" world)))

(defn- format-expr [expr]
  (let [line-info (some-> (:line (meta expr)) (#(str " (at line: " % ")")))]
    (str "'" expr "'" line-info)))

(defn transition->fn-expr [transition-expr]
  `(fn [world#]
     (try
       (valid-world-result (~transition-expr world#) ~(str transition-expr))
       (catch Throwable throwable#
         (timbre/error throwable# :log :transition-exception)
         (fail ~(format-expr transition-expr) "threw exception:\n"
                (pr-str throwable#))))))

(defmulti form->var class)

(defmethod form->var Symbol [s]
  (resolve s))

(defmethod form->var ISeq [l]
  (let [[fst snd] l]
    (cond
      (#{'partial 'comp} fst) (form->var snd)
      :else                   (form->var fst))))

(defmethod form->var :default [_]
  nil)

(defn- is-test-var? [form test-vars]
  (and (symbol? form)
       (-> form resolve meta :test)
       #_(test-vars (resolve form))))
(defn- is-testing? [form]
  (and (coll? form)
       (-> form first name (= "testing"))))
(defn- is-check-ref? [form]
  (and (coll? form)
       (-> form first name (= "testing"))))
(defn- is-query? [form]
  (-> form form->var meta ::query))

(defn- classify [test-vars form]
  (cond
    (is-testing? form)            [:check (testing->fn-expr form) (str form)]
    (is-test-var? form test-vars) [:check (test-var->fn-expr form) (str form)]
    (is-query? form)              [:query (transition->fn-expr form) (str form)]
    :else                         [:transition (transition->fn-expr form) (str form)]))

(s/defn forms->steps :- [Step] [forms :- [Expression], test-vars]
  (->> forms (map (partial classify test-vars)) retry-sequences seq))

(defn get-flow-information
  [flow-name forms metadata]
  (let [[flow-title in-forms] (if (string? (first forms))
                                [(first forms) (rest forms)]
                                [nil forms])
        flow-description      (if flow-title
                                (str flow-name " " flow-title)
                                flow-name)]
    {:flow-description flow-description
     :flow-title       flow-title
     :in-forms         in-forms}))

(defmacro defflow [name & forms]
  (let [{:keys [flow-title
                in-forms
                flow-description]} (get-flow-information name forms (meta &form))]
    `(do (~`t/deftest ~name
                  (->> (list ~@(forms->steps in-forms nil))
                       run-steps
                       ;(announce-results ~flow-description)
                       ))
      (alter-meta! (var ~name) assoc :flow true))))
