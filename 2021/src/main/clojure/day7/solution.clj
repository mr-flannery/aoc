(ns day7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input-file (io/resource "day7/input.txt"))

(def real-input (map #(Integer/parseInt %) (str/split (slurp input-file) #",")))

(def example-input [16 1 2 0 4 2 7 1 2 14])

(defn abs [n] (max n (- n)))

(defn distances-to-position
  [positions target-position]
  (->> positions
       (map #(abs (- target-position %)))
       (reduce +)))

(defn positions-for-input
  [input]
  (range 1 (inc (apply max input))))

(defn part1
  [horizontal-positions]
  (->> (positions-for-input horizontal-positions)
       (map #(distances-to-position horizontal-positions %))
     ;; (map vector (positions-for-input example-input))
     ;; (sort-by second)
       sort
       first))

(println (part1 real-input)) ;; 328318
