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
  (def raw-chan (chan))
  (tap m raw-chan)
  (tap m buckets)
  (defonce dict (atom {}))
  (go (prepare-buckets (<! buckets)))
  (read-dict #(go (->> %2
                       clojure.string/split-lines
                       (map (fn [w] (.toLowerCase w)))
                       (>! c)))))

(defn prepare-dictionary [d]
  (-> (for [i (range 1 (count (first d)))]
         (loop [sub-d d
                words []]
           (if-let [new-d (->> sub-d (drop-while #(.startsWith % (peek words))) seq)]
             (recur new-d (conj words (.substring (first new-d) 0 i)))
             (set words))))
      (conj nil)
      vec
      (conj (set d))))

(defn prepare-buckets [raw]
  (when (-> (count @dict) (= 0))
    (doseq [l (range 2 25)] ;; highest in dict
      (swap! dict assoc l (prepare-dictionary (filter #(= l (count %)) raw))))))

(defn index->coords [i w] [(rem i w) (int (/ i w))])
(defn coords->index [x y w] (+ x (* w y)))

(defn maybe-word?
  [d word]
  (let [possible-word (->> d (drop-while #(-> % (compare word) (< 0))) first)]
    (when possible-word
      (clojure.string/starts-with? possible-word word))))

(defn adjacent
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
(defn next-indices
  "word is the partial
  i is the index of the last letter of word
  ->letter is the vector of letters
  note: i can be mapped to nil or not, it won't be used"
  [i word [w h ->letter] in-dict?]
  (->> (adjacent i w h)
       (filter #(->letter %))
       (filter #(->> (->letter %)
                     (str word)
                     in-dict?))))

(defn use-index [i board]
  (assoc-in board [2 i] nil))

(defn find-words [len word indices board d]
  (let [index->word #(str word (get-in board [2 %]))]
    (if (= len 1)
      (map index->word indices)
      (mapcat
       #(let [new-word (index->word %)
              [new-dict use-word?]
              (if (vector? d)
                [d (-> new-word count inc d)]
                [(drop-while (fn [e] (-> e (compare new-word) (< 0))) d)
                 (partial maybe-word? new-dict)])
              new-indices (next-indices % new-word board use-word?)]
          (find-words (dec len) new-word new-indices (use-index % board) new-dict))
       indices))))

(defn solve [board w h l]
  (let [letters (vec (replace {\- nil} board))]
    (->> (find-words l "" (->> board
                               count
                               range
                               (filter letters))
                     [w h letters]
                     (or (and (contains? @dict l)
                              (@dict l))
                         (filter #(= l (count %)) raw-dict)))
         set
         sort)))

(defn -main [board & lwh]
  (when board
    (go (defonce raw-dict (<! raw-chan))
        (let [[l w h]
              (map int
                   (if (< (count lwh) 3)
                     (list* (first lwh)
                            (repeat 2 (.sqrt js/Math (count board))))
                     lwh))]
          (apply println (solve (.toLowerCase board) w h l))
          (.exit js/process)))))
(set! *main-cli-fn* -main)
