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
(def field (parse input))

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

(let [field sample-field
      max-l (->> field keys (map first) (apply max))
      max-c (->> field keys (map second) (apply max))]
  (-> field
      (play-round max-l max-c)
      (print-field max-l max-c)))

; TODO: but I may need to to either weight moves somehow or say something like "you can never move back"
; p-queue with negative steps?

(defn possible-moves
  [field player-pos max-l max-c]
  (let [[l c] player-pos]
    ; down/right > wait > up/left
    (->> [[(inc l) c] [l (inc c)] [l c] [(dec l) c] [l (dec c)]]
         (filter (fn [[ll cc]] (or
                                 (and (<= 0 ll max-l) (<= 0 cc max-c))
                                 (= [ll cc] [-1 0]))))
         (filter #(empty? (get field %))))))

(comment
  (time
    (let [player-pos      [-1 0]
          field           field
          max-l           (->> field keys (map first) (apply max))
          max-c           (->> field keys (map second) (apply max))
          min-path-length (atom Integer/MAX_VALUE)
          known-states    (HashSet.)]
      (loop [[[player-pos field steps] & states] [[player-pos field 0]]]
        ;(Thread/sleep 10000)
        ;(println (count states) @min-path-length)
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
                (= player-pos [max-l max-c]) (do
                                               (swap! min-path-length #(min % (inc steps)))
                                               (recur states))
                :default
                (let [next-field  (play-round field max-l max-c)
                      p-moves     (possible-moves next-field player-pos max-l max-c)
                      next-states (->> p-moves
                                       (map (fn [move] [move next-field (inc steps)])))]
                  (recur (into (into [] next-states) states)))))))))))


