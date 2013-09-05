(ns pmbauer.text.split
  (:require [clojure.core.reducers :as r]
            [clojure.string :as str])
  (:import [java.util.regex Pattern])
  (:refer-clojure :exclude [chunk]))

(set! *warn-on-reflection* true)

(defn split [^Pattern re s]
  (remove str/blank? (str/split s re)))

(defrecord Chunk [s])
(defrecord Segment [l m r])

(defn chunk
  ([a1] (->Chunk (str a1)))
  ([] (chunk ""))
  ([a1 a2] (chunk (str a1 a2))))

(defn segment
  ([l m r] (->Segment l m r))
  ([] (segment "" [] "")))

(def C-ZERO (chunk))
(def S-ZERO (segment))

(defn maybe-word [s]
  (if (str/blank? s)
    []
    [[s]]))

(defn wordstate->words [{:keys [s l m r] :as word-state}]
  (reduce into []
          (condp = (type word-state)
            Chunk (maybe-word s)
            Segment (reduce into [] [(maybe-word l)
                                     m
                                     (maybe-word r)]))))

(defmulti plus #(map type %&))

(defmethod plus '() []
  C-ZERO)

(defmethod plus [Chunk Chunk] [^Chunk a ^Chunk b]
  (chunk (.-s a) (.-s b)))

(defmethod plus [Chunk Segment] [^Chunk a ^Segment b]
  (segment (str (.-s a) (.-l b))
           (.-m b)
           (.-r b)))

(defmethod plus [Segment Chunk] [^Segment a ^Chunk b]
  (segment (.-l a)
           (.-m a)
           (str (.-r a) (.-s b))))

(defmethod plus [Segment Segment] [^Segment a ^Segment b]
  (segment (.-l a)
           (reduce into [] [(.-m a)
                            (maybe-word (str (.-r a) (.-l b)))
                            (.-m b)])
           (.-r b)))

(def fjinvoke @#'r/fjinvoke)
(def fjfork @#'r/fjfork)
(def fjjoin @#'r/fjjoin)
(def fjtask @#'r/fjtask)

(defn foldstr [^String s n combinef sequential-reducef]
  (let [fc (fn [child] #(sequential-reducef child))
        boundaries (partition 2 1 (range 0 (count s) n))
        f-last (fc (.substring s (or (last (last boundaries)) 0) (count s)))
        jobs (->> boundaries
                  (map #(.substring s (first %) (second %)))
                  (map fc)
                  vec
                  (#(conj % f-last)))]
    (fjinvoke #(->> (map fjtask jobs)
                    (map fjfork)
                    doall
                    (map fjjoin)
                    (reduce combinef)))))

(extend-protocol r/CollFold
  String
  (coll-fold [s n combinef reducef]
    (foldstr s n combinef #(reduce reducef (combinef) %))))

(defn reducer [result ch]
  (plus result (if (= ch \space)
                 S-ZERO
                 (chunk ch))))

(defn split-reducers
  ([s] (split-reducers 8192 s))
  ([chunk-size s]
     (wordstate->words (r/fold chunk-size plus reducer s))))

(defn string->wordstate [^Pattern re ^String s]
  (let [splits (str/split s re)
        splitcount (count splits)]
    (if (= 0 splitcount)
      S-ZERO
      (let [last-split (nth splits (dec splitcount))
            end-match? (not= (.lastIndexOf s ^String last-split)
                             (- (count s) (count last-split)))]
        (if (and (= 1 splitcount) (not end-match?))
          (chunk last-split)
          (segment (first splits)
                   (->> (subvec splits 1 (- splitcount (if end-match? 0 1)))
                        (remove str/blank?)
                        vec
                        vector)
                   (if end-match? "" last-split)))))))

(defn guess-chunk-size [s]
  (max (/ (count s)
          (* 2 (.availableProcessors (Runtime/getRuntime))))
       4096))

(defn psplit
  ([^Pattern re s]
     (psplit re (guess-chunk-size s) s))
  ([^Pattern re chunk-size s]
     (wordstate->words (foldstr s chunk-size plus #(string->wordstate re %)))))
