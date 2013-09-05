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

(def ^:const C-ZERO (chunk))
(def ^:const S-ZERO (segment))

(defn maybe-word [s]
  (if (str/blank? s)
    []
    [s]))

(defn wordstate->words [{:keys [s l m r] :as word-state}]
  (condp = (type word-state)
    Chunk (maybe-word s)
    Segment (concat (maybe-word l) m (maybe-word r))))

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
           (vec (concat (.-m a)
                          (maybe-word (str (.-r a) (.-l b)))
                          (.-m b)))
           (.-r b)))

(def ^:const fjinvoke @#'r/fjinvoke)
(def ^:const fjfork @#'r/fjfork)
(def ^:const fjjoin @#'r/fjjoin)
(def ^:const fjtask @#'r/fjtask)

(defn foldstr [^String s n combinef sequential-reducef]
  (cond
   (empty? s) (combinef)
   (<= (count s) n) (sequential-reducef s)
   :else
   (let [split (quot (count s) 2)
         v1 (.substring s 0 split)
         v2 (.substring s split (count s))
         fc (fn [child] #(foldstr child n combinef sequential-reducef))]
     (fjinvoke #(let [f1 (fc v1)
                      t2 (fjtask (fc v2))]
                  (fjfork t2)
                  (combinef (f1) (fjjoin t2)))))))

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
                        (remove str/blank?))
                   (if end-match? "" last-split)))))))

(defn guess-chunk-size [s]
  (/ (count s) (.availableProcessors (Runtime/getRuntime))))

(defn psplit
  ([^Pattern re s]
     (psplit re (guess-chunk-size s) s))
  ([^Pattern re chunk-size s]
     (wordstate->words (foldstr s chunk-size plus #(string->wordstate re %)))))
