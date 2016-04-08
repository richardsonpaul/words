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

(defn- adjacent
  [i w h]
  (let [[x y] (index->coords i w)
        delta (range -1 2)]
    (for [new-y (map #(+ y %) delta)
          :when (< -1 new-y h)
          new-x (map #(+ x %) delta)
          :when (< -1 new-x w)
          :when (or (not= x new-x) (not= y new-y))]
      (coords->index new-x new-y w))))

;; board: [w h [...]] where the vector acts as an associative for index->letter
(defn- next-indices
  "word is the partial
  i is the index (into the board) of the last letter of word
  ->letter is the vector of letters (the \"board\")
  in-dict? is a fn which will receive a word and say whether it's valid or to abort/prune
  note: i can be mapped to nil on the board, or a letter. Since it's not adjacent to itself,
     it won't matter. Previously used letters should be nil"
  [i word [w h ->letter] in-dict?]
  (->> (adjacent i w h)
       (filter #(->letter %))
       (filter #(->> (->letter %)
                     (str word)
                     in-dict?))))

(defn- use-index [i board]
  (assoc-in board [2 i] nil))

(defn- find-words
  "d should be a dictionary *function*
  pass it a word to get a new fn f:
  invoke f with no args for the new dict to pass to the recursive call
  invoke f with one arg (a word) to use as a pred - use-word?"
  [len word indices board d]
  (let [index->word #(str word (get-in board [2 %]))]
    (if (= len 1)
      (map index->word indices)
      (mapcat
       #(let [new-word (index->word %)
              new-dict (d new-word)
              new-indices (next-indices % new-word board new-dict)]
          (find-words (dec len) new-word new-indices (use-index % board) (new-dict)))
       indices))))

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
     (->> (find-words l "" (->> board
                                count
                                range
                                (filter letters))
                      [w h letters]
                      (dict-fn l optimized))
          set
          sort)))
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
