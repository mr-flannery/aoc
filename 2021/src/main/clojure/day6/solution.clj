(ns day6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def example-input [3 4 3 1 2])

(def input-file (io/resource "day6/input.txt"))

(def real-input (map #(Integer/parseInt %) (str/split (slurp input-file) #",")))

(defn process-fish
  [[fish]]
  (if (= fish 0)
    [6 8]
    [(dec fish)]))

;; something in this naive implementation takes ages
;; could be that apply concat combined with map makes this quadratic...
(defn part1
  [fish days]
  (count (reduce (fn [fish, _] (apply concat (pmap process-fish fish))) fish (range days))))

;; this approach is dumb, it can be solved way easier by simply counting
(defn initial-counts
  [fish]
  (merge-with +
              (into (sorted-map) (zipmap (range 0 9) (repeat 9 0))) 
              (frequencies fish)))

(defn rotate
  [[head & rest]]
  (concat rest [head]))

(defn next-day
  [current-state]
  (let [current (vals current-state)
        next (into (sorted-map) (zipmap (range 0 9) (rotate current)))]
    (merge-with + next {6 (get next 8)})))

(defn part1-2
  [fish days]
  (->> (range days)
       (reduce (fn [state _] (next-day state)) (initial-counts fish))
       vals
       (reduce +)))

;;  (println (part1 real-input 80)) ;; 390923
(part1-2 real-input 256) ;; 1749945484935
