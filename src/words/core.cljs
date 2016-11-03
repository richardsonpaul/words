(ns ^:figwheel-always words.core
  (:require [cljs.nodejs :as node]
            clojure.string
            [cljs.core.async :refer [<! >! chan mult tap]])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(node/enable-util-print!)

(def fs (cljs.nodejs/require "fs"))
(def read-dict #(.readFile fs "/usr/share/dict/words" "utf8" %))
(declare prepare-buckets raw-dict)

(let [c (chan)
      m (mult c)
      buckets (chan)]
  (def ^:private raw-chan (chan))
  (tap m raw-chan)
  (tap m buckets)
  (defonce ^:private dict (atom {}))
  (go (prepare-buckets (<! buckets)))
  (read-dict #(go (->> %2
                       clojure.string/split-lines
                       (map (fn [w] (.toLowerCase w)))
                       (>! c)))))

(defn- prepare-dictionary [d]
  (-> (for [i (range 1 (count (first d)))]
         (loop [sub-d d
                words []]
           (if-let [new-d (->> sub-d (drop-while #(.startsWith % (peek words))) seq)]
             (recur new-d (conj words (.substring (first new-d) 0 i)))
             (set words))))
      (conj nil)
      vec
      (conj (set d))))

(defn- prepare-buckets [raw]
  (when (-> (count @dict) (= 0))
    (doseq [l (range 2 25)] ;; highest in dict
      (swap! dict assoc l (prepare-dictionary (filter #(= l (count %)) raw))))))

(defn- index->coords [i w] [(rem i w) (int (/ i w))])
(defn- coords->index [x y w] (+ x (* w y)))

(def adjacent (memoize (fn[i w h]
                   (let [[x y] (index->coords i w)
                         delta (range -1 2)]
                     (for [new-y (map #(+ y %) delta)
                           :when (< -1 new-y h)
                           new-x (map #(+ x %) delta)
                           :when (< -1 new-x w)
                           :when (or (not= x new-x) (not= y new-y))]
                       (coords->index new-x new-y w))))))

(defn- use-index [i board]
  (assoc-in board [2 i] nil))

(def find-words
  "l is the length of word to find
  s is the board size
  i is the index to search from"
  (memoize
   (fn [l w h i]
     ;; find all adjacent indices, and find all words of n-1 from those. append index to each
     (if (= l 1)
       [(list i)]
       (->> (adjacent i w h)
            (mapcat #(find-words (dec l) w h %))
            (map #(conj % i)))))))

(defn- new-d [d]
  (fn [w]
    (let [skip-words (fn [nw dict] (drop-while #(-> % (compare nw) (< 0)) dict))
          nd (skip-words w d)]
         (fn
           ([] (new-d nd))
           ([nw] (when-let [possible-word (first (skip-words nw nd))]
                   (clojure.string/starts-with? possible-word nw)))))))

(defn- dict-fn [l d]
  (or (when-let [dict (and d (d l))]
        (fn this [_]
          (fn
            ([] this)
            ([w] ((-> w count dict) w)))))
      (new-d (filter #(= l (count %)) raw-dict))))

(defn solve
  ([board w h l optimized]
   (let [letters (vec (replace {\- nil} board))]
     (->> board
          count
          range
;;          (filter letters) ; ?
          (mapcat #(find-words l w h %))
;;          (dict-fn l optimized)
          set
          count
          ;; sort
          )))
  ([b l]
   (let [s (.sqrt js/Math (count b))]
     (solve b s s l)))
  ([b s l]
   (solve b s s l))
  ([b w h l]
   (solve b w h l @dict)))

(defn process-solution
  ([board args]
   (process-solution board (partial apply println) args))
  ([board k args]
   (go (defonce ^:private raw-dict (<! raw-chan))
       (k (apply solve board args)))))

(defn -main [board & whl]
  (when board
    (process-solution (.toLowerCase board)
                      #(do (apply println %)
                           (.exit js/process))
                      (map int whl))))
(set! *main-cli-fn* -main)
