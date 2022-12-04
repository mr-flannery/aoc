(ns day4.solution
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def sample-input "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8")

(defn parse-line
  [line]
  (let [[_ & digits] (re-matches #"(\d+)-(\d+),(\d+)-(\d+)" line)
        [l1 l2 r1 r2] (map #(Integer/parseInt %) digits)]
    [[l1 l2] [r1 r2]]))

(def sample-parsed (map parse-line (str/split-lines sample-input)))

(def input-file (io/resource "day4/input.txt"))

(def input (->> (str/split (slurp input-file) #"\n")
                (map parse-line)))

(defn interval-contains?
  [intervals]
  (let [[[l1 l2] [r1 r2]] (sort intervals)]
    (cond
      (< l1 r1) (>= l2 r2)
      (= l1 r1) (<= l2 r2))))

(defn part1
  [input]
  (->> input
       (filter interval-contains?)
       count))

(defn zip
  [& cols]
  (partition (count cols) (count cols) (apply interleave cols)))

;; part 2

(defn between?
  [a [b c]]
  (and (<= b a) (>= c a)))

(defn interval-overlaps?
  [intervals]
  (let [[[l1 l2] [r1 r2]] (sort intervals)]
    (between? r1 [l1 l2])))

(defn part2
  [input]
  (->> input
       (filter interval-overlaps?)
       count))

(defn -main
  []
  (println "Part 1")
  (println (part1 input))
  (println "Part 2")
  (println (part2 input))
  )

(-main)
