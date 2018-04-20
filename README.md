# postman

_What are postman tests?_

Postman tests are integration-style tests for a single service that use the [`flow`](https://github.com/nubank/postman/blob/master/src/postman/flow.clj#L216-L226) macro.
The entry point for postman tests are the endpoints of the service: http handlers and kafka consumers.
Hence, all internal service code remains un-mocked, but external communications with HTTP, kafka, and other components like S3, redis, etc, are mocked.

Flows follow a world-transition pattern. The flow starts with a base world state, which is an empty map, and each subsequent form in the flow is either a transition, query, or check step.

Given that service code isn't mocked in postman tests, schema validation is enabled by default within the `flow` macro.

The flow structure can also be the basis for end-to-end (`e2e`) style tests. In the case of `e2e` tests, incoming/outgoing correspondences aren't mocked, so flow transitions make can send HTTP requests or produce kafka messages that will be processed by fully spun up services.

### system components

Postman flows are capable of testing a single service's logic, that is, everything that lies between the incoming data (http endpoints and kafka consumer handlers) and outgoing data (http client requests and kafka message production). Thus, to trigger things like message consumption, we need access to the service's various components.

Convention is write an `init!` transition function that initializes the system components and stores it in the world under the `:system` key.

### world

The world is a map that stores:

 * state used by helper functions, for instance, the (mock) http component
 * values to be checked inside of Midje `fact`s
 * intermediate values to be used in future computations

### steps

 * __Transition functions__: a 1-arity function that must take in a world and return a world. They generally have side-effects, store results under keys for checking, and by principal avoid mocking as much as possible.
 * __Checks__: are Midje `fact` or `facts` expressions that should perform checks over values stored in the world. Since facts don't modify the world, or accept a world argument, the world is made available within facts via the `*world*` dynamic variable. Checks are retriable; the `flow` macro will re-run checks that fail until they succeed or a timeout is reached.
 * __Query functions__: retriable transition functions defined using [`postman.flow/defnq`](https://github.com/nubank/postman/blob/master/src/postman/flow.clj#L228-L232) and `fnq`. If running the function fails, it will be retried. This functionality is generally only used in flows for end-to-end tests, when you want to get data from a potentially flaky source like over http.

### simple example

```clojure
(ns postman.example
  (:require [postman.flow :refer [*world* flow]]
            [my-mocks.http :refer [GET]]
            [my-mocks.kafka.mock-consumer :as kafka.mock-consumer]
            [my-service.components :as components]))

(defn init!
  "setup components and store them in the world"
  [world]
  (let [system (components/ensure-system-up!)]
    ;; .. code to setup kafka, http, etc services ..
    (assoc world :system system)))

(defn load-bill
  "Hit service's endpoint to access bill data"
  [bill-id world]
  (let [url (str "/admin/bill/" id "/")]
    (assoc world :bill (GET :json url 200))))

(flow
  ;; the world starts out as an empty map: {}

  ;; transition step that initializes the system components and store them in the world
  init!

  ;; transition step that triggers service code via an http endpoint
  (partial load-bill #uuid "3290571d-09c3-4f08-99ec-a0bad7c4c546")

  ;; check step
  (fact "check the loaded bill"
    (:bill *world*) => {:total 1
                        :name  "Radhia Cousot"})

  ;; transition step that triggers service code via kafka message consumption
  (fn [world]
    (let [message  {:topic   :publish-bill
                    :message {:total 2
                              :name  "Radhia Cousot"}}
          consumer (-> world :system :consumer)]
      (kafka.mock-consumer/consume! message))
      ;; don't forget that transition steps always return a world
      world)

  ;; consuming a message doesn't return anything, but we can check behavior by
  ;; checking messages produced, outgoing http calls, and updated results from
  ;; subsequent incoming http calls
  ...)
```

### probing with retry sequences

Check and query steps within a flow are retriable.
The `flow` macro will group adjacent retriable steps into a sequence.
When one step in a sequence fails, the entire sequence will be retried.
This allows for nice probing behavior:

```clojure
(require '[postman.flow :refer [flow fnq]]
(def counter (atom 0))

(flow "query / check probing example"
  ;; a transition isn't retriable, so it won't get grouped in a sequence
  (fn [w] (println "transition step run once") w)

  ;; a query that starts 'succeeding' after 3 calls
  (fnq [w]
    (if (< (swap! counter inc) 3)
      (do (println "fail query")
          (throw (Exception. "try again")))
      (do (println "pass query")
          w)))

  ;; this check is grouped with the above query into a 'retriable sequence'
  (fact "queried 10 times?" @counter => 10))

;; results in the following output:
;; "transition step run once"
;; "fail query"
;; "fail query"
;; "fail query"
;; "pass query"
;; ... 6x "pass query"
;; true
```

### dynamic variables

 * `*probe-timeout*`: keep retrying query and check steps until this millisecond timeout has elapsed.
 * `*probe-sleep-period*`: time in milliseconds to wait before retrying a query or check step.
 * `*verbose*`: controls whether the step should be logged to stdout and splunk.
 * `*world*`: the current world, which is made available within check steps.
