(ns pmbauer.text.generator
  (:require [clojure.data.generators :as gen]))

(def ascii-alphanumeric
  (vec (map char (concat (range 48 58)       ;; numbers
                         (range 65 91)       ;; uppercase
                         (range 97 123)))))  ;; lowercase

(defn default-word-sizer []
  (gen/uniform 1 17))

(defn word
  ([] (word default-word-sizer))
  ([sizer]
     (gen/string #(gen/rand-nth ascii-alphanumeric) sizer)))

(defn space
  ([] (space (gen/uniform 1 3)))
  ([sizer]
     (gen/string (constantly \space) sizer)))

(defn corpus
  "Generates a random string with a given number of words
  sepparated by variable whitespace"
  ([word-count] (corpus word-count default-word-sizer))
  ([word-count word-sizer]
     (apply str (take (dec (* 2 word-count))
                      (interleave (repeatedly #(word word-sizer))
                                  (repeatedly space))))))
