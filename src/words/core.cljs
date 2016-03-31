(ns ^:figwheel-always words.core
  (:require [cljs.nodejs :as node]
            clojure.string))

(node/enable-util-print!)

(def fs (cljs.nodejs/require "fs"))
(def read-dict #(.readFile fs "/usr/share/dict/words" "utf8" %))
(def raw-dict (clojure.string/split (.readFileSync fs "/usr/share/dict/words" "utf8") "\n"))

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
  [i word [w h ->letter] dict]
  (->> (adjacent i w h)
       (filter #(->letter %))
       (filter #(->> (->letter %)
                     (str word)
                     (maybe-word? dict)))))

(defn use-index [i board]
  (assoc-in board [2 i] nil))

(defn solve [len word indices board dict]
  (let [index->word #(str word (get-in board [2 %]))]
    (if (= len 1)
      (map index->word indices)
      (mapcat
       #(let [new-word (index->word %)
              new-dict (drop-while (fn [e] (-> e (compare new-word) (< 0))) dict)
              new-indices (next-indices % new-word board new-dict)]
          (solve (dec len) new-word new-indices (use-index % board) new-dict))
       indices))))

(defn find-words [l w h board]
  (let [letters (vec (replace {\- nil} board))]
    (->> (solve l "" (->> board
                          count
                          range
                          (filter letters))
                [w h letters]
                (filter #(= (count %) l) raw-dict))
         set
         ;; (remove nil?)
         sort)))

;; define your app data so that it doesn't get over-written on reload

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)

(defn -main [board & lwh] (let [[l w h]
                                (map int
                                     (if (< (count lwh) 3)
                                       (list* (first lwh)
                                              (repeat 2 (.sqrt js/Math (count board))))
                                       lwh))]
                            (time (find-words l w h board))))
(set! *main-cli-fn* -main)
