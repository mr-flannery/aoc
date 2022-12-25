(ns day24.solution
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.set :refer [intersection union]])
  (:import (java.util HashSet ArrayList)))

(def input (slurp (io/resource "day24/input.txt")))

(def sample-input "#E######\n#>>.<^<#\n#.<..<<#\n#>v.><>#\n#<^v^^>#\n######.#")

(defn parse
  [input]
  (let [lines (str/split-lines input)]
    (into {} (for [l (range 0 (- (count lines) 2))
                   c (range 0 (- (count (first lines)) 2))]
               (let [field (-> lines (nth (inc l)) (nth (inc c)))]
                 (if (= field \.)
                   [[l c] []]
                   [[l c] [field]]))))))

(def sample-field (parse sample-input))
(def real-field (parse input))

(defn next-blizzard
  [[l c] blizzards max-l max-c]
  (->> (for [b blizzards]
         (condp = b
           \^ [[(mod (dec l) (inc max-l)) c] [\^]]
           \v [[(mod (inc l) (inc max-l)) c] [\v]]
           \< [[l (mod (dec c) (inc max-c))] [\<]]
           \> [[l (mod (inc c) (inc max-c))] [\>]]))
       (into {})))

(def play-round
  (memoize
    (fn play-round
      [field max-l max-c]
      (->> field
           (map (fn [[pos blizzards]] (next-blizzard pos blizzards max-l max-c)))
           (apply merge-with concat)))))

(defn play-n-rounds
  [field n max-l max-c]
  (loop [i     0
         field field]
    (if (= i n)
      field
      (recur (inc i) (play-round field max-l max-c)))))

(defn print-field
  [field max-l max-c]
  (->> (for [l (range 0 (inc max-l))
             c (range 0 (inc max-c))]
         (let [val (get field [l c])]
           (cond
             (= 0 (count val)) \.
             (= 1 (count val)) (first val)
             :default (count val))))
       (partition (inc max-c))
       (map #(apply str %))
       (mapv println))
  nil)

(defn possible-moves
  [field player-pos max-l max-c]
  (let [[l c] player-pos]
    ; TODO: depending on the order of the list the results are different...
    (->> [[(inc l) c] [l (inc c)] [(dec l) c] [l (dec c)] [l c]]
         (filter (fn [[ll cc]] (or
                                 (and (<= 0 ll max-l) (<= 0 cc max-c))
                                 (= [ll cc] [-1 0])
                                 (= [ll cc] [(inc max-l) max-c]))))
         (filter #(empty? (get field %))))))

; for part2, I need to know the state of the board for which I found the shortest path
; which will always be the same because the board state is purely a function of minutes 🤦
(defn find-shortest-path
  [field starting-pos]
  (let [player-pos      starting-pos
        max-l           (->> field keys (map first) (apply max))
        max-c           (->> field keys (map second) (apply max))
        target-pos      (if (= starting-pos [-1 0]) [max-l max-c] [0 0])
        min-path-length (atom Integer/MAX_VALUE)
        known-states    (HashSet.)]
    (loop [[[player-pos field steps] & states] [[player-pos field 0]]]
      ;(Thread/sleep 10000)
      (println (count states) @min-path-length)
      (let [new-state (hash-unordered-coll [player-pos field])]
        (if (.contains known-states new-state)
          (do
            ;(println "known state!")
            (recur states))
          (do
            (.add known-states new-state)
            (cond
              (nil? player-pos) @min-path-length
              (>= steps @min-path-length) (recur states)
              (= player-pos target-pos) (do
                                          (swap! min-path-length #(min % (inc steps)))
                                          (recur states))
              :default (let [next-field  (play-round field max-l max-c)
                             p-moves     (possible-moves next-field player-pos max-l max-c)
                             next-states (->> p-moves
                                              (map (fn [move] [move next-field (inc steps)])))]
                         ; changing this from (into next-states states) to (into states next-states) fixed the p-moves order bug
                         ; interesting side effect seems to be that I don't find a result for a long time and then once I find one it's the shortest
                         ; similar to as if I would use a p-queue that prioritizes shorter paths
                         (recur (into (into [] states) next-states))))))))))

(comment
  (time
    (let [field      field
          max-l      (->> field keys (map first) (apply max))
          max-c      (->> field keys (map second) (apply max))
          there      (find-shortest-path field [-1 0])
          back       (find-shortest-path (play-n-rounds field there max-l max-c) [(inc max-l) max-c])
          back-again (find-shortest-path (play-n-rounds field (+ there back) max-l max-c) [-1 0])]
      (println "there: " there ", back: " back ", back again: " back-again)
      (+ there back back-again)))

  (time
    (let [field sample-field
          max-l (->> field keys (map first) (apply max))
          max-c (->> field keys (map second) (apply max))
          back  (find-shortest-path (play-n-rounds field 18 max-l max-c) [(inc max-l) max-c])]
      back)))

(print-field (play-n-rounds sample-field 20 3 5) 3 5)
