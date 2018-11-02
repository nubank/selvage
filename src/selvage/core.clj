(ns selvage.core
  (:require [selvage.visibility :as vis]
            [schema.core :as s]
            [taoensso.timbre :as timbre])
  (:import [clojure.lang ISeq Symbol]))

(def ^:dynamic *quiet* false)
(def ^:dynamic *verbose* false)
(def ^:dynamic *report-fail*)

(def worlds-atom (atom {}))

(defn stdout-emit [& strings]
  (when-not *quiet*
    (apply print strings)
    (flush)))

(defn emit-ln [message log-map]
  (timbre/info :log-map log-map)
  (stdout-emit (format "%-70s\t\t\t[CID: %s]\n" message (vis/current-cid))))

(defn emit-debug-ln [message log-map]
  (when *verbose*
    (emit-ln message log-map)))

(defn announce-flow [flow-description]
  (emit-debug-ln (str "Running flow: " flow-description)
                 {:flow-description flow-description
                  :log              :flow/start}))

(defn save-world-debug! [name world]
  (swap! worlds-atom assoc name world)
  world)

(def Expression s/Any)
(def Step [(s/one (s/enum :transition :retry) 'kind)
           (s/one Expression 'expression)
           (s/one s/Str 'description)])

(defn timed-apply [run-function & args]
  (let [start   (System/nanoTime)
        ret     (apply run-function args)
        elapsed (/ (double (- (System/nanoTime) start)) 1000000.0)]
    [elapsed ret]))


(defn retry-expr [retry [_kind f-expr desc]]
  `[:retry (fn [w#] ((~retry ~f-expr) w#)) ~desc])

(defn- partition-group-by [pred coll]
  (->> coll (partition-by pred) (map #(vector (pred (first %)) %))))

(defn retriable-step? [[kind _f _desc]]
  (-> kind #{:check :query} boolean))

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

(defn steps-to-step [steps]
  `[:sequence (fn [w#] (run-step-sequence [w# ""] (list ~@steps))) "running multiple steps"])

(defn retry-sequences [retry-fn steps]
  (->> steps
       (partition-group-by retriable-step?)
       (mapcat (fn [[retriable-seq? steps]]
                 (if retriable-seq?
                   [(retry-expr retry-fn (steps-to-step steps))]
                   steps)))))

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

(defn is-query? [form]
  (or (-> form form->var meta :selvage.flow/query)
      (-> form form->var meta :selvage.cflow/query)))

(s/defn forms->steps :- [Step]
  [classify-fn, retry-fn, forms :- [Expression]]
  (->> forms (map classify-fn) (retry-sequences retry-fn) seq))

(defn run-steps [steps]
  (reset! worlds-atom {})
  (run-step-sequence [{} ""] steps))

(defn- format-expr [expr]
  (let [line-info (some-> (:line (meta expr)) (#(str " (at line: " % ")")))]
    (str "'" expr "'" line-info)))

(defn fail [expr-str details & failure-messages]
  (*report-fail*)
  [false (apply str "\033[0;33m  Step " expr-str " " details " \033[0m " failure-messages)])

(defn valid-world-result [world expr-str]
  (if (map? world)
    [world ""]
    (fail expr-str "did not result in a map (i.e. a valid world):\n" world)))

(defn transition->fn-expr [transition-expr]
  `(fn [world#]
     (try
       (valid-world-result (~transition-expr world#) ~(str transition-expr))
       (catch Throwable throwable#
         (timbre/error throwable# :log :transition-exception)
         (fail ~(format-expr transition-expr) "threw exception:\n"
                (pr-str throwable#))))))
