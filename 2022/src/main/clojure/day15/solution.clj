(ns day15.solution
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]])
  (:import (java.util HashSet)))

(def input (slurp (io/resource "day15/input.txt")))

(def sample-input "Sensor at x=2, y=18: closest beacon is at x=-2, y=15\nSensor at x=9, y=16: closest beacon is at x=10, y=16\nSensor at x=13, y=2: closest beacon is at x=15, y=3\nSensor at x=12, y=14: closest beacon is at x=10, y=16\nSensor at x=10, y=20: closest beacon is at x=10, y=16\nSensor at x=14, y=17: closest beacon is at x=10, y=16\nSensor at x=8, y=7: closest beacon is at x=2, y=10\nSensor at x=2, y=0: closest beacon is at x=2, y=10\nSensor at x=0, y=11: closest beacon is at x=2, y=10\nSensor at x=20, y=14: closest beacon is at x=25, y=17\nSensor at x=17, y=20: closest beacon is at x=21, y=22\nSensor at x=16, y=7: closest beacon is at x=15, y=3\nSensor at x=14, y=3: closest beacon is at x=15, y=3\nSensor at x=20, y=1: closest beacon is at x=15, y=3")

(defn parse
  [input]
  (->> (str/split-lines input)
       (map #(re-matches #"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)" %))
       (map (fn [[_ sx sy bx by]] [[(Integer/parseInt sx) (Integer/parseInt sy)] [(Integer/parseInt bx) (Integer/parseInt by)]]))))

(def sample-sensor-beacon-pairs (parse sample-input))
(def sensor-beacon-pairs (parse input))

(defn abs
  [n]
  (max n (- n)))

(defn distance
  [[sx sy] [bx by]]
  (+ (abs (- sx bx)) (abs (- sy by))))

; part 1 - non-performant approach
(comment
  (defn points-covered
    [[sx sy] [bx by]]
    (let [d (distance [sx sy] [bx by])]
      (for [x (range (- sx d) (+ sx d 1))
            y (range (- sy d) (+ sy d 1))
            :when (<= (distance [sx sy] [x y]) d)
            :when (not (= [x y] [bx by]))]
        [x y])))

  (let [input    sensor-beacon-pairs
        target-y 2000000]
    (->> input
         (map #(apply points-covered %))
         (reduce #(into %1 %2) #{})
         (filter (fn [[x y]] (= y target-y)))
         count)))

; better approach:
; calculate the distance
; check if target-y is in there
; explicitly generate the relevant points
; and then we're basically done

(defn points-covered2
  [[sx sy] [bx by] target-y]
  (let [d (distance [sx sy] [bx by])]
    (if (<= (- sy d) target-y (+ sy d))
      (let [sy-ty-d            (abs (- (abs sy) (abs target-y)))
            remainaing-x-steps (- d sy-ty-d)]
        (for [x (range (- sx remainaing-x-steps) (+ sx remainaing-x-steps 1))
              :when (not (= [x target-y] [bx by]))] ; TODO: refactor
          [x target-y]))
      [])))

(let [input    sample-sensor-beacon-pairs
      target-y 10]
  (->> input
       (map (fn [[s b]] (println s b) (points-covered2 s b target-y)))
       (reduce #(into %1 %2) #{})
       count
       ))

(let [input    sensor-beacon-pairs
      target-y 2000000]
  (->> input
       (map (fn [[s b]] (points-covered2 s b target-y)))
       (reduce #(into %1 %2) #{})
       count
       ))
