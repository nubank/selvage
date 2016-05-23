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

(defn transition->fn-expr [transition-expr]
  `(fn [world#]
     (try [(~transition-expr world#) ""]
       (catch Throwable throwable#
         (fact (throw throwable#) => (str "Step '" ~(str transition-expr) "' to not throw an exception"))
         [false throwable#]))))

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
  (run-step-sequence [{} ""] steps))

(defn format-result [flow-description [success? desc]]
  (when-not success?
    (emit desc))
  (emit-debug-ln (str "Running flow: " flow-description))   ; Emit before
  (emit-debug-ln "Flow finished" (if success?
                                   "succesfully"
                                   "with failures"))
  (emit-debug "\n")
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

(defmacro fnq [& forms]
  `^::query (fn ~@forms))

(defmacro defnq [name & forms]
  `(def ~name ^::query (fn ~@forms)))
