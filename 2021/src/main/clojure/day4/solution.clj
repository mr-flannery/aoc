(ns day4
  (:require [clojure.pprint :refer [pprint]]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def example-draws [7 4 9 5 11 17 23 2 0 14 21 24 10 16 13 6 15 25 12 22 18 20 8 19 3 26 1])

(def example-boards [[[22 13 17 11  0]
                      [8  2 23  4 24]
                      [21  9 14 16  7]
                      [6 10  3 18  5]
                      [1 12 20 15 19]]

                     [[3 15  0  2 22]
                      [9 18 13 17  5]
                      [19  8  7 25 23]
                      [20 11 10 24  4]
                      [14 21 16 12  6]]

                     [[14 21 17 24  4]
                      [10 16 15  9 19]
                      [18  8 23 26 20]
                      [22 11 13  6  5]
                      [2  0 12  3  7]]])

(def input-file (io/resource "day4/input.txt"))

(def real-draws
  (map #(Integer/parseInt %) (-> (slurp input-file)
                                 (str/split #"\n")
                                 first
                                 (str/split #","))))

(def real-boards (->> (-> (slurp input-file)
                          (str/split #"\n")
                          rest)
                      (filter #(not (= "" %)))
                      (mapcat #(str/split % #" "))
                      (filter #(not (= "" %)))
                      (map #(Integer/parseInt %))
                      (partition 5)
                      (partition 5)))

;; let is straight destructuring, i.e. rows is still a 2d vector
;; (let [rows (first boards)]
;;   (map (fn [num] [num false]) rows))

;; for will destructure element-wise
;; i.e. it will execute the expression for each element of board
(defn reset-board
  [board]
  (for [row board]
    (map (fn [num] [num false]) row)))

(defn prepare-boards
  [boards]
  (map reset-board boards))

(defn mark-number
  [draw board]
  (for [row board]
    (map (fn [[num mark]]
           (if (= num draw)
             [num true]
             [num mark]))
         row)))

(defn transpose
  [matrix]
  (apply mapv vector matrix))

(defn has-winning-row?
  [board]
  ((comp not empty?) (filter #(every? identity %)
                             (for [row board]
                               (map (fn [[_ mark]] mark) row)))))

(defn has-winning-column?
  [board]
  (has-winning-row? (transpose board)))

(defn play-round
  [{:keys [winner boards winning-draw]} draw]
  (if (some? winner)
    {:winner winner :boards boards :winning-draw winning-draw}
    (let [next-boards (map #(mark-number draw %) boards)
          ;; assuming the input is set up to only give one definitive winner
          winners (concat (filter has-winning-row? next-boards) (filter has-winning-column? next-boards))]
      (if (empty? winners)
        {:winner nil :boards next-boards :winning-draw nil}
        {:winner (first winners) :boards next-boards :winning-draw draw}))))

(defn play-game
  [boards draws]
  (reduce play-round {:winner nil :boards boards :winning-draw nil} draws))

(defn final-score
  [{:keys [winner winning-draw]}]
  (* (->> winner
          (mapcat identity)
          (filter (fn [[_ mark]] (= mark false)))
          (map (fn [[num _]] num))
          (reduce +))
     winning-draw))

(defn part1
  [boards draws]
  (final-score (play-game (prepare-boards boards) draws)))

(defn -main
  []
  (print (part1 real-boards real-draws))) ;; 49686

(-main)

