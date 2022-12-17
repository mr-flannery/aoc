(ns day17.solution
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]])
  (:import (java.util HashSet)))

(def input (str/trim (slurp (io/resource "day17/input.txt"))))

(def sample-input ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")

(defn parse
  [input]
  (->> (str/split-lines input)
       (map #(re-matches #"Valve (.{2}) has flow rate=(\d+); tunnels? leads? to valves? (.*)" %))
       (reduce (fn [graph [_ valve flow neighbors]] (assoc graph valve {:f (Integer/parseInt flow) :n (map str/trim (str/split neighbors #","))})) {})))

; TODO: these may need to be sets instead of vectors
(defn horizontal-line
  [bot-y]
  #{[2 bot-y] [3 bot-y] [4 bot-y] [5 bot-y]})

(defn plus
  [bot-y]
  #{[3 (+ 2 bot-y)]
    [2 (+ 1 bot-y)] [3 (+ 1 bot-y)] [4 (+ 1 bot-y)]
    [3 bot-y]})

(defn reverse-l
  [bot-y]
  #{[4 (+ 2 bot-y)]
    [4 (+ 1 bot-y)]
    [2 bot-y] [3 bot-y] [4 bot-y]})

(defn vertical-line
  [bot-y]
  #{[2 (+ 3 bot-y)] [2 (+ 2 bot-y)] [2 (+ 1 bot-y)] [2 bot-y]})

(defn block
  [bot-y]
  #{[2 (+ 1 bot-y)] [3 (+ 1 bot-y)]
    [2 bot-y] [3 bot-y]})

(def piece-fns [horizontal-line plus reverse-l vertical-line block])

(defn rotate
  [[h & t]]
  (concat t [h]))

; TODO: I need to check for collision with other pieces
(defn jet-left
  [piece]
  (->> piece
       (map (fn [[x y]] [(dec x) y]))
       (into #{})))

(defn jet-right
  [piece]
  (->> piece
       (map (fn [[x y]] [(inc x) y]))
       (into #{})))

(defn apply-jet
  [jet pile piece]
  (if (= jet \<)
    (if (and
          (not= (->> piece (map first) (apply min)) 0)
          (= 0 (count (clojure.set/intersection (->> pile (apply concat) (into #{})) (jet-left piece)))))
      (jet-left piece)
      piece)
    (if (and
          (not= (->> piece (map first) (apply max)) 6)
          (= 0 (count (clojure.set/intersection (->> pile (apply concat) (into #{})) (jet-right piece)))))
      (jet-right piece)
      piece)))

(defn fall
  [piece]
  (->> piece
       (map (fn [[x y]] [x (dec y)]))
       (into #{})))

(defn can-fall?
  [pile piece]
  (and
    (= 0 (count (clojure.set/intersection (->> pile (apply concat) (into #{})) (fall piece))))
    (not= 0 (->> piece (map second) (apply min)))))

(defn next-piece
  [piece-fns top-of-pile]
  (let [top-of-pile-y (if (some? top-of-pile)
                        (->> top-of-pile
                             (map second)
                             (apply max))
                        -1)]
    ((first piece-fns) (+ 4 top-of-pile-y))))

; apparently concat is lazy and produces StackOverflowErrors ?
(loop [pile      []
       piece-fns piece-fns
       jets      input
       piece     (next-piece piece-fns (first pile))]
  (if (= (count pile) 2022)
    pile
    (let [jetted-piece (apply-jet (first jets) pile piece)]
      ;(println (first jets))
      ;(println (sort jetted-piece))
      (if (can-fall? pile jetted-piece)
        (recur pile piece-fns (rotate jets) (fall jetted-piece))
        (let [new-pile      (concat [jetted-piece] pile)
              new-piece-fns (rotate piece-fns)
              new-piece     (next-piece new-piece-fns (first new-pile))]
          ;(println (sort (first new-pile)))
          (recur new-pile new-piece-fns (rotate jets) new-piece))))))

