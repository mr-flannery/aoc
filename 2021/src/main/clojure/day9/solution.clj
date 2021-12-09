(ns day9
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def example-input [[2 1 9 9 9 4 3 2 1 0]
                    [3 9 8 7 8 9 4 9 2 1]
                    [9 8 5 6 7 8 9 8 9 2]
                    [8 7 6 7 8 9 6 7 8 9]
                    [9 8 9 9 9 6 5 6 7 8]])

(def input-file (io/resource "day9/input.txt"))

(def real-input (for [row (->> (str/split (slurp input-file) #"\n")
                               (map #(str/split % #"")))]
                  (map #(Integer/parseInt %) row)))

(defn get-point
  [m x y]
  (-> m
      (nth y)
      (nth x)))

;; this feels very imperative...
;; wait, this version has diagonals
(defn neighbor-coords-w-diag
  [m x y]
  (let [x-max (dec (count (first m)))
        y-max (dec (count m))
        x-coords (cond
                   (= x 0) [x (inc x)]
                   (= x x-max) [(dec x) x]
                   :else [(inc x) x (dec x)])
        y-coords (cond
                   (= y 0) [y (inc y)]
                   (= y y-max) [(dec y) y]
                   :else [(inc y) y (dec y)])]
    (remove #{[x y]} (for [x' x-coords
                           y' y-coords]
                       [x' y']))))

(defn neighbor-coords
  [m x y]
  (let [x-max (dec (count (first m)))
        y-max (dec (count m))
        x-neighbors (cond
                      (= x 0) [[(inc x) y]]
                      (= x x-max) [[(dec x) y]]
                      :else [[(dec x) y] [(inc x) y]])
        y-neighbors (cond
                      (= y 0) [[x (inc y)]]
                      (= y y-max) [[x (dec y)]]
                      :else [[x (dec y)] [x (inc y)]])]
    (concat x-neighbors y-neighbors)))

;; part1
(->> (let [input real-input]
       (for [y (range (count input))
             x (range (count (first input)))]
         (let [neighbors (->> (neighbor-coords input x y)
                              (map (fn [[x y]] (get-point input x y))))
               height (get-point input x y)]
           (if (every? #(> % height) neighbors)
             height
             -1))))
     flatten
     (filter (comp not neg?))
     (map inc)
     (reduce +)) ;; 570

;; get all basins
(defn basins
  [input]
  (->> (for [y (range (count input))
             x (range (count (first input)))]
         (let [neighbors (->> (neighbor-coords input x y)
                              (map (fn [[x y]] (get-point input x y))))
               height (get-point input x y)]
           (if (every? #(> % height) neighbors)
             [x y])))
       (filter (comp not nil?))))

(defn basin-neighbors
  [input [x y]]
  (let [neighbors (neighbor-coords input x y)
        height (get-point input x y)]
    (filter #(and (< (apply get-point input %) 9) (>= (apply get-point input %) height)) neighbors)))

(defn all-basin-points
  [input basin-root]
  (loop [candidates [basin-root]
         basin [basin-root]]
    (if (empty? candidates)
      basin
      (let [next (set (remove (set basin) (mapcat #(basin-neighbors input %) candidates)))]
        (recur next (concat basin next))))))

(let [input real-input
      basins (basins input)]
  (->> basins
       (map #(all-basin-points input %))
      ;;  (map (fn [coords] (map #(apply get-point input %) coords)))
       (map count)
       (sort >)
       (take 3)
       (reduce *))) ;; 899392
