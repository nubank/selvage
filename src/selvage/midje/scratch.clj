(ns selvage.midje.scratch
  (:require [selvage.midje.flow :refer [flow *world* *load-flow*]]
            [midje.sweet :refer :all]))

(defn add-hello [world]
  (assoc world :message "Hello"))

(defn add-world [world]
  (Thread/sleep 1000)
  (update world :message #(str % " world!")))

(binding [*load-flow* true]
  (flow "testing"
        (partial add-hello)
        (partial add-world)
        (fact "making sure"
              *world* => {:message "Hello world"})))

#_(binding [*load-flow* false]
  (flow "testing again"
        (partial add-hello)
        (partial add-world)
        (fact "making sure"
              *world* => {:message "Hello world"})))

#_(time
 (->> *ns*
          ns-map
          vals
          (filter #(:test (meta %)))
#_          (map (partial clojure.test/test-var))
#_          doall
          ))
