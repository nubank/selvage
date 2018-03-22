(ns postman.formatting
  (:require [io.aviso.exception :as aviso.exception]
            [io.aviso.ansi :as aviso.ansi]))

(defn format-exception [throwable]
  (binding [aviso.exception/*traditional* true
            aviso.exception/*fonts*       (merge aviso.exception/*fonts*
                                                 {:message       aviso.ansi/white-font
                                                  :clojure-frame aviso.ansi/white-font
                                                  :function-name aviso.ansi/white-font})]
    (aviso.exception/format-exception throwable)))
