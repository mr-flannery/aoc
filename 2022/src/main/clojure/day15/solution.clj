(ns day15.solution
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]])
  (:import (java.util HashSet BitSet)))

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
(defn points-covered
  [[sx sy] [bx by]]
  (let [d (distance [sx sy] [bx by])]
    (for [x (range (- sx d) (+ sx d 1))
          y (range (- sy d) (+ sy d 1))
          :when (<= (distance [sx sy] [x y]) d)
          ;:when (not (= [x y] [bx by]))
          ]
      [x y])))

(comment
  (let [input    sensor-beacon-pairs
        target-y 2000000]
    (->> input
         (map #(apply points-covered %))
         ;(reduce #(into %1 %2) #{})
         ;(filter (fn [[x y]] (= y target-y)))
         ;count
         ))

  (def all-covered-points
    (let [input    sensor-beacon-pairs
          target-y 2000000]
      (->> input
           (pmap #(apply points-covered %))
           ;(reduce #(into %1 %2) #{})
           ;(filter (fn [[x y]] (= y target-y)))
           ;count
           ))))

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
              :when (not (= [x target-y] [bx by]))] ;this check my be inefficient. I know the point, I can remove it by hand. or rather I can just remove all the beacons by hand
          [x target-y]))
      [])))

(let [input    sample-sensor-beacon-pairs
      target-y 10]
  (->> input
       (map (fn [[s b]] (println s b) (points-covered2 s b target-y)))
       (reduce #(into %1 %2) #{})
       count
       ))

; part1
(comment
  (let [input    sensor-beacon-pairs
        target-y 2000000]
    (->> input
         (map (fn [[s b]] (points-covered2 s b target-y)))
         (reduce #(into %1 %2) #{})
         count
         )))

; for part2, I need to find all places that are not covered
; I can use the naive approach and then reduce it by intersection
; i.e. all points not covered by any sensor-beacon-pair

; okay this approach might not work
(comment
  (defn points-not-covered
    [[sx sy] [bx by]]
    (let [d (distance [sx sy] [bx by])]
      (for [x (range (- sx d) (+ sx d 1))
            y (range (- sy d) (+ sy d 1))
            :when (> (distance [sx sy] [x y]) d)
            ;:when (not (= [x y] [bx by]))
            ]
        [x y])))

  (let [input    sample-sensor-beacon-pairs
        target-y 10]
    (->> input
         (map #(apply points-not-covered %))
         (map #(into #{} %))
         (apply concat)
         (filter #(= % [14 11]))
         ))
  (points-not-covered [8 7] [2 10])
  )

; why does points-covered2 take so long?
(defn points-covered3
  [[sx sy] [bx by] target-y min-coord max-coord ary]
  (let [d (distance [sx sy] [bx by])]
    (if (<= (- sy d) target-y (+ sy d))
      (let [sy-ty-d            (abs (- (abs sy) (abs target-y)))
            remainaing-x-steps (- d sy-ty-d)]
        (doall
          ; for is lazy 
          (for [x (range (max min-coord (- sx remainaing-x-steps)) (min max-coord (+ sx remainaing-x-steps 1)))]
            (do
              (println x target-y)
              (.add ary [x target-y])))))
      [])))

; this already takes too long and runs out of memory 
(comment
  (let [input   sample-sensor-beacon-pairs
        min     0
        max     20
        covered (java.util.HashSet.)]
    (loop [[[s b] & others] input]
      (if (nil? s)
        covered
        (do
          (dotimes [i max]
            (println i)
            (points-covered3 s b i min max covered))
          (recur others)
          )
        ))
    ))

(defn is-covered-by?
  [scanner beacon point]
  (<= (distance scanner point) (distance scanner beacon)))

(comment
  (time
    (let [points (java.util.HashSet.)
          input  sensor-beacon-pairs
          max    4000000]
      (dotimes [x max]
        (dotimes [y max]
          (if-not (->> input
                       (map #(is-covered-by? (first %) (second %) [x y]))
                       (reduce #(or %1 %2) false))
            (do (println x y) (.add points [x y])))))
      points)))

(defn points-covered-1-1
  [s b min-coord max-coord]
  (let [[sx sy] s
        [bx by] b
        d     (distance [sx sy] [bx by])
        min-x (max min-coord (- sx d))
        max-x (min max-coord (+ sx d))
        min-y (max min-coord (- sy d))
        max-y (min max-coord (+ sy d))
        ]
    (println min-x max-x min-y max-y)
    (loop [i      min-x
           points #{}]
      (if (= i (inc max-x))
        points
        (let [num-field-left-right (- d (abs (- i sx)))]
          (recur (inc i) (into points (for [y (range
                                                (max min-y (- sy num-field-left-right))
                                                (inc (min max-y (+ sy num-field-left-right))))]
                                        [i y]))))))))

(defn points-covered-1-1-bin
  [s b min-coord max-coord bitset-m]
  (let [[sx sy] s
        [bx by] b
        d     (distance [sx sy] [bx by])
        min-x (max min-coord (- sx d))
        max-x (min max-coord (+ sx d))
        min-y (max min-coord (- sy d))
        max-y (min max-coord (+ sy d))
        ]
    (println min-x max-x min-y max-y)
    (loop [i min-y]
      (if (= i (inc max-y))
        nil
        (do
          (let [num-field-left-right (- d (abs (- i sy)))
                bitset               (doto (BitSet. 4000001)
                                       (.set (max min-x (- sx num-field-left-right))
                                             (inc (min max-x (+ sx num-field-left-right)))))]
            (update bitset-m i #(.or % bitset))
            (recur (inc i))))))))



; part2 sample
(comment
  (let [min-coord   0
        max-coord   20
        bitset-m    (into {} (for [i (range 0 21)]
                               [i (BitSet. 21)]))
        comp-bitset (doto (BitSet. 21) (.set 0 21))]
    (time
      (loop [[[s b] & r] sample-sensor-beacon-pairs]
        (if (nil? s)
          nil
          (do
            (points-covered-1-1-bin s b min-coord max-coord bitset-m)
            (recur r)))))
    (let [[y bs] (first (filter (fn [[k v]] (not= v comp-bitset)) bitset-m))
          x (.nextClearBit bs 0)]
      (+ y (* x 4000000)))
    ))

;part2 real
(comment
  (let [min-coord   0
        max-coord   4000000
        bitset-m    (into {} (for [i (range 0 4000001)]
                               [i (BitSet. 4000001)]))
        comp-bitset (doto (BitSet. 4000001) (.set 0 4000001))]
    (time
      (loop [[[s b] & r] sensor-beacon-pairs]
        (if (nil? s)
          nil
          (do
            (points-covered-1-1-bin s b min-coord max-coord bitset-m)
            (recur r)))))
    (let [[y bs] (first (filter (fn [[k v]] (not= v comp-bitset)) bitset-m))
          x (.nextClearBit bs 0)]
      (+ y (* x 4000000)))
    ))

;; BitSets are too inefficient. storing 4e6 bits for 4e6 roles requires almost 2TB memory, if my calculations are right
;; maybe I can try using intervals

(defn points-covered-intervals
  [s b min-coord max-coord]
  (let [[sx sy] s
        [bx by] b
        d     (distance [sx sy] [bx by])
        min-x (max min-coord (- sx d))
        max-x (min max-coord (+ sx d))
        min-y (max min-coord (- sy d))
        max-y (min max-coord (+ sy d))

        ]
    (println min-x max-x min-y max-y)
    (loop [i          min-y
           interval-m {}]
      (if (= i (inc max-y))
        interval-m
        (do
          (let [num-field-left-right (- d (abs (- i sy)))]
            (recur (inc i) (assoc interval-m i [[(max min-x (- sx num-field-left-right)) (inc (min max-x (+ sx num-field-left-right)))]]))))))))

(defn merge-intervals
  [intervals]
  (try
    (reduce (fn
              [col-or-interval [from2 to2]]
              ;(println [col-or-interval [from2 to2]])
              (if (number? (first col-or-interval))
                (let [[from1 to1] col-or-interval]
                  (cond
                    (<= from1 from2 to1 to2) [from1 to2]
                    (<= from1 from2 to2 to1) [from1 to1]
                    :default [[from1 to1] [from2 to2]]))
                (let [col-or-interval (sort-by first col-or-interval)
                      col (drop-last col-or-interval)
                      [from1 to1] (last col-or-interval)]
                  (cond
                    (<= from1 from2 to1 to2) (into col [[from1 to2]])
                    (<= from1 from2 to2 to1) (into col [[from1 to1]])))
                )) intervals)
    (catch Exception e
      (println intervals)
      (throw e)
      )))

(merge-intervals '([0 847280] [0 1739084] [761555 804402] [801521 1835044] [1179374 2218999] [1713813 2091280] [2154966 2331037] [2281518 2353747] [2353746 2727057] [2396854 2727057] [2727058 3252741] [2727058 3213933] [2790355 2797328] [3213932 3502955] [3285796 3619495] [3285796 4000001] [3473554 3883549] [3898238 3966357]))

(comment
  (time
    (let [min-coord 0
          max-coord 20]
      (->> (loop [[[s b] & r] sample-sensor-beacon-pairs
                  int-m {}]
             (if (nil? s)
               int-m
               (recur r (merge-with #(into %1 %2) int-m (points-covered-intervals s b min-coord max-coord)))))
           (sort-by first)
           (map (fn [[k v]] [k (sort-by first v)]))
           (map (fn [[k v]] [k (merge-intervals v)]))
           (filter (fn [[k v]] (not (number? (first v)))))
           )

      ))
  )

(def part2-intervals
  (memoize
    (fn
      [sensor-beacon-pairs min-coord max-coord]
      (loop [[[s b] & r] sensor-beacon-pairs
             int-m {}]
        (if (nil? s)
          int-m
          (recur r (merge-with #(into %1 %2) int-m (points-covered-intervals s b min-coord max-coord))))))))

;part2.1
(comment
  (time
    (let [sensor-beacon-pairs sensor-beacon-pairs
          min-coord           0
          max-coord           4000000]
      (->> (part2-intervals sensor-beacon-pairs min-coord max-coord)
           (sort-by first)
           ;(drop 2916500)
           ;(take 125)
           (map (fn [[k v]] [k (sort-by first v)]))
           (map (fn [[k v]] [k (merge-intervals v)]))
           ;(filter nil?)
           (filter (fn [[k v]] (not (number? (first v)))))
           ))
    ))

;part2.2
(comment
  (let [[y intervals] [2916597 '([2727058 4000001] [0 2727057])]
        x (-> (sort intervals) first second)]
    (+ y (* x 4000000))
  )


(->> '([0 13] [6 11] [12 15] [14 21])
     (reduce (fn [[from1 to1] [from2 to2]]
               (println [[from1 to1] [from2 to2]])
               (cond
                 (<= from1 from2 to1 to2) [from1 to2]
                 (<= from1 from2 to2 to1) [from1 to1]))))

