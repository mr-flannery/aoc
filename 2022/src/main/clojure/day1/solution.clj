(ns day1.solution
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def sample-input [[1000
                    2000
                    3000]
                   [4000]
                   [5000
                    6000]
                   [7000
                    8000
                    9000]
                   [10000]])

(def input-file (io/resource "day1/input.txt"))

(defn int-or-nil
  [s]
  (try
    (Integer/parseInt s)
    (catch Exception e
      nil)))

(def input (->> (str/split (slurp input-file) #"\n")
                (map int-or-nil)
                (partition-by some?)
                (filter (fn [col] (some? (first col))))
                ))

(defn part1
  [input]
  (apply max (map (fn [col] (reduce + col)) input)))

(defn part2
  [input]
  (->> input
       (map (fn [col] (reduce + col)))
       (sort #(compare %2 %1))
       (take 3)
       (reduce +)))

(defn -main
  []
  (println "Part 1")
  (println (part1 input))
  (println "Part 2")
  (println (part2 input)))

(-main)
