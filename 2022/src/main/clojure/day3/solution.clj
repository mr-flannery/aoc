(ns day3.solution
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def sample-input ["vJrwpWtwJgWrhcsFMMfFFhFp"
                   "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
                   "PmmdzqPrVvPwwTWBwg"
                   "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
                   "ttgJtRGJQctTZtZT"
                   "CrZsJsPPZsGzwwsLwLmpwMDw"])

(def input-file (io/resource "day3/input.txt"))

(def input (->> (str/split (slurp input-file) #"\n")))

(defn find-common-char
  [s]
  (let [first-half (subs s 0 (/ (count s) 2))
        second-half (subs s (/ (count s) 2) (count s))]
    (first (clojure.set/intersection (set first-half) (set second-half)))))

(def priorities (zipmap (seq "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") (range 1 53)))

(defn part1
  [input]
  (->> input
       (map find-common-char)
       (map priorities)
       (reduce +)))

;; part 2

(defn find-badge
  [[s1 s2 s3]]
  (first (clojure.set/intersection (set s1) (set s2) (set s3))))

(defn part2
  [input]
  (->> (partition 3 3 input)
       (map find-badge)
       (map priorities)
       (reduce +)))

(defn -main
  []
  (println "Part 1")
  (println (part1 input))
  (println "Part 2")
  (println (part2 input))
  )

(-main)
