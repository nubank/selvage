(ns common-test.postman.flow
  (:require [schema.core :as s]
            [midje.sweet :refer [fact facts anything]]
            [midje.emission.api :as m-emission]
            [midje.emission.state :as m-state]
            [common-core.visibility :as vis]
            [clojure.string :as str])
  (:import [java.io StringWriter ByteArrayOutputStream PrintStream]))

(def ^:dynamic *probe-timeout* 300)
(def ^:dynamic *probe-sleep-period* 10)
(def ^:dynamic *verbose* false)
(def ^:dynamic *world* {})

(def worlds-atom (atom {}))

(defn emit [& strings]
  (when (m-emission/config-above? :print-nothing)
    (apply print strings)))

(defn emit-ln [& strings]
  (emit (format "%-70s\t\t\t[CID: %s]\n" (str/join " " strings) (vis/current-cid))))

(defn emit-debug [& strings]
  (when *verbose* (apply emit strings)))

(defn emit-debug-ln [& strings]
  (emit-debug (format "%-70s\t\t\t[CID: %s]\n" (str/join " " strings) (vis/current-cid))))

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

(defn check->fn-expr [check-expr]
  `(fn [world#]
     (let [writer# (new StringWriter)]
       (binding [*world* world#
                 clojure.test/*test-out* writer#]
         (let [result#  ~check-expr
               success# (when result#
                          world#)]
           [success# (str writer#)])))))

(defn print-exception-string [exception]
  (let [output-baos (ByteArrayOutputStream.)]
    (.printStackTrace exception (PrintStream. output-baos))
    (String. (.toByteArray output-baos) "UTF-8")))

(defn transition->fn-expr [transition-expr]
  `(fn [world#]
     (try [(~transition-expr world#) ""]
       (catch Throwable throwable#
         (m-state/output-counters:inc:midje-failures!)
         [false (str "Step '" ~(str transition-expr) "' threw exception:\n" (print-exception-string throwable#))]))))

(defn retriable? [[kind f desc]]
  (-> kind #{:check :query} boolean))

(def max-retries 5)

(defn resetting-midje-counters [f]
  (let [output-counters-before (m-state/output-counters)]
    (fn [& args]
      (m-state/set-output-counters! output-counters-before)
      (apply f args))))

(defn timed-apply [run-function & args]
  (let [start   (. System (nanoTime))
        ret     (apply run-function args)
        elapsed (/ (double (- (. System (nanoTime)) start)) 1000000.0)]
    [elapsed ret]))

(defn retry? [elapsed-millis]
  (<= elapsed-millis *probe-timeout*))

(defn retry [f]
  (letfn [(retry-f [elapsed-so-far f w]
            (let [[time [success? desc :as res]] (timed-apply f w)
                  elapsed (+ elapsed-so-far time)]
              (if success?
                res
                (if (retry? elapsed)
                  (do
                    (emit-debug "x")
                    (Thread/sleep *probe-sleep-period*)
                    (retry-f (+ elapsed *probe-sleep-period*) f w)) ;TODO: improve time accounting
                  [false desc]))))]
    (partial retry-f 0 (resetting-midje-counters f))))


(defn partition-group-by [pred coll]
  (->> coll (partition-by pred) (map #(vector (pred (first %)) %))))

(defn run-step [[world _] [step-type f desc]]
  (vis/with-split-cid
    (do
      (emit-debug-ln "Running " (format "%-10s" (name step-type)) " " desc)
      (let [[next-world desc] (f world)]
        (if next-world
          [next-world desc]
          (reduced [next-world desc]))))))

(defn run-step-sequence [s0 steps]
  (reduce run-step s0 steps))

(defn steps-to-step [steps]
  `[:sequence (fn [w#] (run-step-sequence [w# ""] (list ~@steps))) "running multiple steps"])

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
          (is-query? [form]
            (if (symbol? form)
              (try (-> form eval meta ::query) (catch Exception _ false))
              (-> form macroexpand meta ::query)))
          (classify  [form] (cond (is-check? form) [:check      (check->fn-expr form) (fact-desc form)]
                                  (is-query? form) [:query      (transition->fn-expr form) (str form)]
                                  :else            [:transition (transition->fn-expr form) (str form)]))]
    (->> forms (map classify) retry-sequences seq)))

(defn run-steps [steps]
  (vis/with-split-cid "FLOW"
    (run-step-sequence [{} ""] steps)))

(defn announce-results [flow-description [success? desc]]
  (when-not success?
    (emit-ln desc))
  (emit-debug-ln "Flow " flow-description " finished"
                 (if success?
                   "succesfully"
                   "with failures"))
  (emit-debug "\n")
  (boolean success?))

(defn wrap-with-metadata [flow-name flow-expr]
  `(s/with-fn-validation
     (facts :postman ~flow-name
       ~flow-expr)))

(defn announce-flow [flow-description]
  (emit-debug-ln (str "Running flow: " flow-description)))

(defmacro flow [& forms]
  (let [flow-name (str (ns-name *ns*)
                       ":"
                       (:line (meta &form)))
        [flow-description in-forms] (if (string? (first forms))
                                      [(str flow-name " " (first forms)) (rest forms)]
                                      [flow-name forms])]
    (wrap-with-metadata flow-description
                        `(do
                           (announce-flow ~flow-description)
                           (->> (list ~@(forms->steps-exprs in-forms))
                                run-steps
                                (announce-results ~flow-description))))))

(defmacro fnq [& forms]
  `^::query (fn ~@forms))

(defmacro defnq [name & forms]
  `(def ~(with-meta name {::query true}) ^::query (fn ~@forms)))

