(ns pmbauer.test.text.split
  (:require [clojure.core.reducers :as r]
            [clojure.string :as str]
            [clojure.test :refer [is are deftest testing]]
            [pmbauer.text.generator :refer [corpus]]
            [pmbauer.text.split :refer [split split-reducers psplit]]))

(def base-fixtures ;; input -> expected
  {"" []
   " " []
   "  " []
   "a" ["a"]
   "at" ["at"]
   "  the quick brown  fox jumps " ["the" "quick" "brown" "fox" "jumps"]})

(deftest run-base-fixtures
  (doseq [[input expected] base-fixtures
          chunk-size [1 2 3 Integer/MAX_VALUE]
          splitter [(partial split #"\s")
                    (partial split-reducers chunk-size)
                    (partial psplit #"\s" chunk-size)]]
    (is (= expected (splitter input))
        (str "splitter:" splitter "\nchunk-size:" chunk-size))))

(deftest all-splitters-equivalent-for-random-corpus
  (is (-> (juxt (partial split-reducers)
                (partial split #"\s")
                (partial psplit #"\s")
                (partial psplit #"\s" 64))
          (map (repeatedly #(corpus 500)))
          (->> (take 10)
               (map #(apply = %))
               (every? true?)))))
