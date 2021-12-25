(ns day20
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def example-input-image ["#..#."
                          "#...."
                          "##..#"
                          "..#.."
                          "..###"])

(def example-algorithm "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#")

(def input-file (io/resource "day20/input.txt"))

(def real-algorithm (first (str/split (slurp input-file) #"\n")))
(def real-image (let [[_ _ & image] (str/split (slurp input-file) #"\n")]
                  (into [] image)))
(defn enhance
  [alg default-char image]
  (let [dim     (count image)
        indices (for [y (range -1 (inc dim))
                      x (range -1 (inc dim))]
                  (->> (for [y1 (range (dec y) (+ y 2))
                             x1 (range (dec x) (+ x 2))]
                         (let [pixel (or (get-in image [y1 x1]) default-char)]
                           (if (= pixel \#)
                             "1"
                             "0")))
                       (apply str)))]
    ;; (println indices)
    (->> indices
         (map #(Integer/parseInt % 2))
         (map #(get alg %))
         (partition (+ dim 2))
         (map #(apply str %))
         (into []))))

(defn two-step
  [algorithm image]
  (enhance real-algorithm \# (enhance real-algorithm \. image)))

;; part1
(->> real-image
     (two-step real-algorithm)
     (apply str)
     (filter #(= % \#))
     count)

;; part2
(->> real-image
     (iterate (partial two-step real-image))
     (take 26)
     last
     (apply str)
     (filter #(= % \#))
     count)

(enhance real-algorithm (enhance real-algorithm real-image))