(ns day23.solution
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.set :refer [intersection union]])
  (:import (java.util HashSet)))

(def input (slurp (io/resource "day23/input.txt")))

(def sample-input "..............\n..............\n.......#......\n.....###.#....\n...#...#.#....\n....#...##....\n...#.###......\n...##.#.##....\n....#..#......\n..............\n..............\n..............\n")

(defn parse
  [input]
  (let [lines (str/split-lines input)]
    (->> (for [i (range 0 (count lines))
               j (range 0 (count (first lines)))]
           (if (= \# (-> lines (nth i) (nth j)))
             [j i])) ; switch j i to get proper x y coords
         (filter identity)
         (into #{}))))

(def sample-elves (parse sample-input))
(def elves (parse input))

(defn north-neighbors
  [[x y]]
  [[(dec x) (dec y)] [x (dec y)] [(inc x) (dec y)]])

(defn south-neighbors
  [[x y]]
  [[(dec x) (inc y)] [x (inc y)] [(inc x) (inc y)]])

(defn east-neighbors
  [[x y]]
  [[(inc x) (dec y)] [(inc x) y] [(inc x) (inc y)]])

(defn west-neighbors
  [[x y]]
  [[(dec x) (dec y)] [(dec x) y] [(dec x) (inc y)]])

(def neighbor-fns [north-neighbors south-neighbors east-neighbors west-neighbors])

(defn rotate
  [[h & t]]
  (into (into [] t) [h]))

(defn proposal
  [elves elf neighbor-fns]
  (let [[x y] elf]
    (if-let [prop (->> neighbor-fns
                       (map #(% [x y]))
                       (filter (fn [pos] (empty? (filter #(.contains elves %) pos))))
                       first)]
      (second prop))))

(defn moves
  [elves]
  (let [proposals (->> elves
                       (map (fn [elf] [elf (proposal elves elf neighbor-fns)]))
                       (filter second)
                       (into {}))]
    (filter (fn [[k v]]
              (= 1 (count (filter #(= % v) (vals proposals)))))
            proposals)))

(defn play-n-rounds
  [elves n]
  (loop [turn  0
         elves elves]
    (if (= turn n)
      elves
      (recur (inc turn) (reduce (fn [elves [from to]] (-> elves (disj from) (conj to))) elves (moves elves))))))

(defn free-spots
  [elves]
  (let [min-x (->> elves (map first) (apply min))
        max-x (->> elves (map first) (apply max))
        min-y (->> elves (map second) (apply min))
        max-y (->> elves (map second) (apply max))]
    (println min-x max-x min-y max-y)
    (-
      (*
        (- (inc max-x) min-x)
        (- (inc max-y) min-y))
      (count elves))))

(defn draw-elves
  [elves]
  (let [min-x (->> elves (map first) (apply min))
        max-x (->> elves (map first) (apply max))
        min-y (->> elves (map second) (apply min))
        max-y (->> elves (map second) (apply max))]
    (->> (for [x (range min-x (inc max-x))
               y (range min-y (inc max-y))]
           (if (.contains elves [x y])
             \#
             \.))
         (partition (- (inc max-x) min-x))
         (map #(apply str %))
         (map println ))))

(draw-elves sample-elves)
