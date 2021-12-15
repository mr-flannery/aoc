(ns day13
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def example-points [[6,10]
                     [0,14]
                     [9,10]
                     [0,3]
                     [10,4]
                     [4,11]
                     [6,0]
                     [6,12]
                     [4,1]
                     [0,13]
                     [10,12]
                     [3,4]
                     [3,0]
                     [8,4]
                     [1,10]
                     [2,14]
                     [8,10]
                     [9,0]])

(def input-file (io/resource "day13/input.txt"))

(defn parse-points
  [lines]
  (for [[x y] (map #(str/split % #",") lines)]
    [(Integer/parseInt x) (Integer/parseInt y)]))

(defn parse-instructions
  [lines]
  (for [[_ dim at] (map #(re-find #".*(x|y)=(\d+)" %) lines)]
    [dim (Integer/parseInt at)]))

(def real-input (let [lines (str/split (slurp input-file) #"\n")
                      points (parse-points (filter #(re-matches #"^\d.*" %) lines))
                      instructions (parse-instructions (filter #(re-matches #"^f.*" %) lines))]
                  [points, instructions]))

(def real-points (first real-input))
(def real-instructions (second real-input))

(defn grid-size
  [points]
  (let [xs (map first points)
        ys (map second points)]
    [(apply max xs) (apply max ys)]))

(defn coerce-vec
  [m]
  (into [] (map vec) m))

(defn grid-for-points
  [grid]
  (let [[x-max y-max] (grid-size grid)]
    (coerce-vec (repeat (inc y-max) (repeat (inc x-max) false)))))

(defn print-grid
  [grid]
  (clojure.pprint/pprint
   (for [y grid]
     (map #(if % 1 0) y))))

(defn set-point
  [m val [x y]]
  (assoc-in m [y x] val))

(defn apply-points-to-grid
  [points]
  (let [grid (grid-for-points points)]
    (reduce (fn [grid point] (set-point grid true point)) grid points)))

(defn transpose
  [m]
  (apply map vector m))

;; y-fold at n means that the nth row will disappear
;; basically it becomes the crease
;; for part2 I think the assumption that folding will always take place at the half point is wrong
(defn abs
  [n]
  (apply max [n (* -1 n)]))

(defn pad-grid
  [grid at-row]
  (let [
        x (count (first grid))
        mid (/ (dec (count grid)) 2)
        diff (abs (- mid at-row))
        extra-rows (repeat (* 2 diff) (repeat x false))]
    (if (< at-row mid)
      (concat extra-rows grid)
      (concat grid extra-rows))))

(defn y-fold
  [grid at-row]
  (let [grid (pad-grid grid at-row)
        upper (take at-row grid)
        lower (take at-row (reverse grid))]
    (->> (map vector upper lower)
         (map transpose)
         (map #(map (fn [[first second]] (or first second)) %)))))

;; (print-grid (let [grid (apply-points-to-grid example-points)
;;       x (count (first grid))
;;       mid (/ (dec (count grid)) 2)
;;       at-row 7
;;       diff (abs (- mid at-row))
;;       extra-rows (repeat (* 2 diff) (repeat x false))]
;;   (if (< at-row mid)
;;     (do-y-fold (concat extra-rows grid) (+ mid diff))
;;     (do-y-fold (concat grid extra-rows) (+ mid diff)))))

(defn x-fold
  [grid at-row]
  (-> grid
      transpose
      (y-fold at-row)
      transpose))

(defn fold
  [grid [dim at-row]]
  (if (= dim "x")
    (x-fold grid at-row)
    (y-fold grid at-row)))

(defn count-dots
  [paper]
  (-> paper
      flatten
      frequencies
      (get true)))

;; example input fold
(-> (apply-points-to-grid example-points)
    (y-fold 7)
    ;; (x-fold 5)
    ;; print-grid
    count-dots)

;; part1
(-> (apply-points-to-grid real-points)
    (fold (first real-instructions))
    count-dots) ;; 669

;; part2
(reduce fold (apply-points-to-grid real-points) real-instructions)

;; #     #   # # # #   # # # #   # # # #     # #     #     #     # #         # #  
;; #     #   #         #               #   #     #   #     #   #     #         #  
;; #     #   # # #     # # #         #     #         #     #   #               #  
;; #     #   #         #           #       #         #     #   #               #  
;; #     #   #         #         #         #     #   #     #   #     #   #     #  
;;   # #     # # # #   #         # # # #     # #       # #       # #       # #    

