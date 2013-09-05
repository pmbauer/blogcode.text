(ns user
  (:require [clojure.repl :refer :all]
            [clojure.string :as str]
            [clojure.tools.namespace.repl :only [refresh]]
            [criterium.core :refer :all]
            [pmbauer.text.split :refer [split psplit split-reducers guess-chunk-size]
             :as split]
            [pmbauer.text.generator :refer [corpus]]))

(comment

;; avg word length = (1 + 16) / 2 = 8.5
;; avg space length = (1 + 2) / 2 = 1.5
;; avg length of text = (avg word length + avg space length) * nr of words
;; at 16 bits per word 1 million words is 16 * ~10 * 1 million = ~19MB
(time (def text (corpus 1000000)))

(with-progress-reporting (bench (def splits (into [] (split #"\s" text))) :verbose))
(with-progress-reporting (bench (def splits (psplit #"\s" text)) :verbose)))
