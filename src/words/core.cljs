(ns ^:figwheel-always words.core
  (:require [cljs.nodejs :as node]
            clojure.string))

;; (enable-console-print!)

(node/enable-util-print!)

(def fs (cljs.nodejs/require "fs"))
(def read-dict #(.readFile fs "/usr/share/dict/words" "utf8" %))
(def raw-dict (clojure.string/split (.readFileSync fs "/usr/share/dict/words" "utf8") "\n"))

;; s implies "size": the size of the board array aka (count board)
;; l implies "length": the length of a size, i.e. (sqrt s)

(defn index->coords [i w] [(rem i w) (int (/ i w))])
(defn coords->index [x y w] (+ x (* w y)))

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
  [i word [w h ->letter] dict]
  (->> (adjacent i w h)
       (filter #(->letter %))
       (filter #(->> (->letter %)
                     (str word)
                     dict))))

(defn use-index [i board]
  (assoc-in board [2 i] nil))

(defn solve [len word indices board dict]
  (let [index->word #(->> [2 %] (get-in board) (str word))]
    (if (= len 1)
      (map index->word indices)
      (mapcat
       #(let [new-word (index->word %)
              new-indices (next-indices % new-word board (-> new-word
                                                             count
                                                             inc
                                                             dict))]
          (solve (dec len) new-word new-indices (use-index % board) dict))
       indices))))

(defn prepare-dictionary [l]
  (let [dict (->> raw-dict
                  (filter #(= l (count %)))
                  (map #(.toLowerCase %)))]
    (->> (for [i (range 1 (inc l))]
           (loop [d (map #(.substr % 0 i) dict)
                  sub-d [(first d)]]
             (let [existing (peek sub-d)
                   new-dict (drop-while #(-> % (compare existing) (< 1)) d)]
               (if (seq new-dict)
                 (recur new-dict (conj sub-d (first new-dict)))
                 sub-d))))
         (cons dict)
         (map set)
         vec)))

(defn find-words [l w h board]
  (let [letters (vec (replace {\- nil} board))
        dict (prepare-dictionary l)]
    (->> (solve l "" (->> board
                          count
                          range
                          (filter letters))
                [w h letters]
                dict)
         (filter (dict 0))
         set
         sort)))

;; define your app data so that it doesn't get over-written on reload

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)

(defn -main [board & whl] (let [[w h l] (map int whl)]
                            (time (find-words l w h board))))
(set! *main-cli-fn* -main)
