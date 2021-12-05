(ns day5
  (:require [clojure.pprint :refer [pprint]]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def example-lines [[[0 9] [5 9]]
                    [[8 0] [0 8]]
                    [[9 4] [3 4]]
                    [[2 2] [2 1]]
                    [[7 0] [7 4]]
                    [[6 4] [2 0]]
                    [[0 9] [2 9]]
                    [[3 4] [1 4]]
                    [[0 0] [8 8]]
                    [[5 5] [8 2]]])

(defn grid-size
  [lines]
  (let [maxs (for [line lines]
               (apply mapv max line))
        xs (map first maxs)
        ys (map first maxs)]
    [(apply max xs) (apply max ys)]))

(defn grid-for-lines
  [lines]
  (into [] (map vec) (let [[x y] (grid-size lines)]
                       (repeat (inc y) (repeat (inc x) 0)))))

(def example-grid (grid-for-lines example-lines))

(def input-file (io/resource "day5/input.txt"))

(defn parse-points
  [[p1 p2]]
  (let [[x1 y1] (map #(Integer/parseInt %) (str/split p1 #","))
        [x2 y2] (map #(Integer/parseInt %) (str/split p2 #","))]
    [[x1 y1] [x2 y2]]))

(def real-lines (->> (str/split (slurp input-file) #"\n")
                     (map #(str/split % #" -> "))
                     (map parse-points)))

(def real-grid (grid-for-lines real-lines))

(defn is-straight?
  [[[x1 y1] [x2 y2]]]
  (or (= x1 x2) (= y1 y2)))

(defn points-in-diagonal-line
  [[[x1 y1] [x2 y2]]]
  (let [xs (if (< x1 x2)
             (range x1 (inc x2))
             (range x1 (dec x2) -1))
        ys (if (< y1 y2)
             (range y1 (inc y2))
             (range y1 (dec y2) -1))]
    (map vector xs ys)))

(defn points-in-straight-line
  [[[x1 y1] [x2 y2]]]
  (if (= x1 x2)
    (let [ymin (first (sort [y1 y2]))
          ymax (second (sort [y1 y2]))]
      (map (fn [y] [x1 y]) (range ymin (inc ymax))))
    (let [xmin (first (sort [x1 x2]))
          xmax (second (sort [x1 x2]))]
      (map (fn [x] [x y1]) (range xmin (inc xmax))))))

(defn get-at
  [grid [x y]]
  (nth (nth grid x) y))

(defn apply-point-to-grid
  [grid point]
  (assoc-in grid point (inc (get-at grid point))))

(defn draw-lines
  [grid lines]
  (reduce
   (fn [grid point] (apply-point-to-grid grid point))
   grid
   (mapcat identity (map points-in-straight-line lines)))) ;; note: straight lines

;; x and y are swapped when printing because that's just how a seq of seqs is displayed
;; could be fixed with transpose

(defn part1
  [grid lines]
  (->> (draw-lines grid (filter is-straight? lines))
       flatten
       (filter #(>= % 2))
       count))

(defn draw-lines-part2
  [grid lines]
  (let [straight-lines (get (group-by is-straight? lines) true)
        diagonal-lines (get (group-by is-straight? lines) false)
        all-points (concat (map points-in-straight-line straight-lines) (map points-in-diagonal-line diagonal-lines))]
    (reduce
     (fn [grid point] (apply-point-to-grid grid point))
     grid
     (mapcat identity all-points))))

(defn part2
  [grid lines]
  (->> (draw-lines-part2 grid lines)
       flatten
       (filter #(>= % 2))
       count))

(print (part1 real-grid real-lines)) ;; 7468
(print (part2 real-grid real-lines)) ;; 22364
