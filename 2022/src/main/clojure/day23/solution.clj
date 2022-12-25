(ns day23.solution
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.set :refer [intersection union]])
  (:import (java.util HashSet)))

(def input (slurp (io/resource "day23/input.txt")))

(def sample-input "..............\n..............\n.......#......\n.....###.#....\n...#...#.#....\n....#...##....\n...#.###......\n...##.#.##....\n....#..#......\n..............\n..............\n..............\n")
(def mini-sample-input ".....\n..##.\n..#..\n.....\n..##.\n.....")

(defn parse
  [input]
  (let [lines (str/split-lines input)]
    (->> (for [l (range 0 (count lines))
               c (range 0 (count (first lines)))]
           (if (= \# (-> lines (nth l) (nth c)))
             [l c]))
         (filter identity)
         (into #{}))))

(def sample-elves (parse sample-input))
(def mini-sample-elves (parse mini-sample-input))
(def elves (parse input))

(defn north-neighbors
  [[l c]]
  [[(dec l) (dec c)] [(dec l) c] [(dec l) (inc c)]])

(defn south-neighbors
  [[l c]]
  [[(inc l) (dec c)] [(inc l) c] [(inc l) (inc c)]])

(defn east-neighbors
  [[l c]]
  [[(dec l) (inc c)] [l (inc c)] [(inc l) (inc c)]])

(defn west-neighbors
  [[l c]]
  [[(dec l) (dec c)] [l (dec c)] [(inc l) (dec c)]])

(defn all-neighbors
  [[l c]]
  [[(dec l) (dec c)] [(dec l) c] [(dec l) (inc c)]
   [l (dec c)] [l (inc c)]
   [(inc l) (dec c)] [(inc l) c] [(inc l) (inc c)]])

(def neighbor-fns [north-neighbors south-neighbors west-neighbors east-neighbors])

(defn rotate
  [[h & t]]
  (into (into [] t) [h]))

(defn proposal
  [elves elf neighbor-fns]
  (if-not (->> (all-neighbors elf)
               (filter #(.contains elves %))
               empty?)
    (if-let [prop (->> neighbor-fns
                       (map #(% elf))
                       (filter (fn [pos] (empty? (filter #(.contains elves %) pos))))
                       first)]
      (do
        ;(println elf (second prop))
        (second prop)))))

(defn moves
  [elves neighbor-fns]
  (let [proposals (->> elves
                       (map (fn [elf] [elf (proposal elves elf neighbor-fns)]))
                       (filter second)
                       (into {}))]
    ;(println (count elves) )
    (filter (fn [[k v]]
              (= 1 (count (filter #(= % v) (vals proposals)))))
            proposals)))

(defn play-n-rounds
  [elves n]
  (loop [turn         0
         elves        elves
         neighbor-fns neighbor-fns]
    (if (= turn n)
      elves
      (recur
        (inc turn)
        (reduce (fn [elves [from to]] (-> elves (disj from) (conj to))) elves (moves elves neighbor-fns))
        (rotate neighbor-fns)))))

(defn free-spots
  [elves]
  (let [min-l (->> elves (map first) (apply min))
        max-l (->> elves (map first) (apply max))
        min-c (->> elves (map second) (apply min))
        max-c (->> elves (map second) (apply max))]
    (println min-l max-l min-c max-c)
    (-
      (*
        (- (inc max-l) min-l)
        (- (inc max-c) min-c))
      (count elves))))

(defn draw-elves
  [elves]
  (let [min-l (->> elves (map first) (apply min))
        max-l (->> elves (map first) (apply max))
        min-c (->> elves (map second) (apply min))
        max-c (->> elves (map second) (apply max))]
    (->> (for [l (range min-l (inc max-l))
               c (range min-c (inc max-c))]
           (if (.contains elves [l c])
             \#
             \.))
         (partition (- (inc max-c) min-c))
         (map #(apply str %))
         (mapv println))))

(comment
  (for [i (range 0 11)]
    (do
      (println (str "round " i ":"))
      (draw-elves (play-n-rounds sample-elves i))
      (println "============"))))

(comment
  (for [i (range 0 4)]
    (do
      (println (str "round " i ":"))
      (draw-elves (play-n-rounds mini-sample-elves i))
      (println "============"))))

;part1
(comment
  (time
    (free-spots (play-n-rounds elves 10))))

(defn play-until-end
  [elves]
  (loop [turn         0
         elves        elves
         neighbor-fns neighbor-fns]
    (let [next-moves (moves elves neighbor-fns)]
      (if (empty? next-moves)
        (inc turn)
        (recur
          (inc turn)
          (reduce (fn [elves [from to]] (-> elves (disj from) (conj to))) elves (moves elves neighbor-fns))
          (rotate neighbor-fns))))))

(comment 
  (time 
    (play-until-end elves)))
