(ns day17.solution
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.set :refer [intersection union]])
  (:import (java.util HashSet)))

(def input (str/trim (slurp (io/resource "day17/input.txt"))))

(def sample-input ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")

(defn parse
  [input])

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
  (doall ; concat is lazy, calling rotate too often in the real input causes StackOverflowError 
    (concat t [h])))

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
          (= 0 (count (intersection (->> pile (reduce union)) (jet-left piece)))))
      (jet-left piece)
      piece)
    (if (and
          (not= (->> piece (map first) (apply max)) 6)
          (= 0 (count (intersection (->> pile (reduce union)) (jet-right piece)))))
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
    (= 0 (count (intersection (->> pile (reduce union)) (fall piece))))
    (not= 0 (->> piece (map second) (apply min)))))

(defn top-of-pile
  [pile]
  (if (not= 0 (count pile))
    (->> pile (reduce union) (map second) (apply max))
    -1))

(defn next-piece
  [piece-fns pile]
  ((first piece-fns) (+ 4 (top-of-pile pile))))

(->> (loop [pile      '()
            piece-fns piece-fns
            jets      input
            piece     (next-piece piece-fns pile)]
       (if (= (count pile) 2022)
         pile
         (let [jetted-piece (apply-jet (first jets) pile piece)]
           (if (can-fall? pile jetted-piece)
             (recur pile piece-fns (rotate jets) (fall jetted-piece))
             (let [new-pile      (conj pile jetted-piece)
                   new-piece-fns (rotate piece-fns)
                   new-piece     (next-piece new-piece-fns new-pile)]
               (recur new-pile new-piece-fns (rotate jets) new-piece))))))
     (reduce union)
     (map second)
     (apply max)
     inc ; the task seems to start counting at 1, not at 0
     )
