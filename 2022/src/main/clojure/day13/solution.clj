(ns day13.solution
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]])
  (:import (java.util HashSet)))

(def input (slurp (io/resource "day13/input.txt")))

(def sample-input "[1,1,3,1,1]\n[1,1,5,1,1]\n\n[[1],[2,3,4]]\n[[1],4]\n\n[9]\n[[8,7,6]]\n\n[[4,4],4,4]\n[[4,4],4,4,4]\n\n[7,7,7,7]\n[7,7,7]\n\n[]\n[3]\n\n[[[]]]\n[[]]\n\n[1,[2,[3,[4,[5,6,7]]]],8,9]\n[1,[2,[3,[4,[5,6,0]]]],8,9]")

(defn parse
  [input]
  (->> (str/split input #"\n\n")
       (map str/split-lines)
       (map (fn [[s1 s2]] [(read-string s1) (read-string s2)]))))

(def sample-packet-pairs (parse sample-input))
(def packet-pairs (parse input))

; if both elements are ints, left must be lower than right
; if both elements are lists, check if both lists are valid
; a list is valid if all of its elements are valid (-> recursive)
; if one value is a list and one value is an integer, convert the integer into one-element list containing the int
; if I understand the task correctly, the result of the total comparison is the result of the first check that we can clearly determine

; 
(defn zip
  [& cols]
  (partition (count cols) (count cols) (apply interleave cols)))

(defn abs
  [n]
  (max n (- n)))

(defn compare-numbers
  [left right]
  (cond
    (< left right) true
    (> left right) false
    (= left right) :cont)) ; nil means continue

(defn cont?
  [x]
  (= x :cont))

(defn pad-lists
  [left right]
  (if (< (count left) (count right))
    [(concat left (repeat (- (count right) (count left)) :cont)) right]
    [left (concat right (repeat (- (count left) (count right)) :cont))]))

(defn compare-lists
  [left right]
  ; TODO: padding
  (let [[left right] (pad-lists left right)]
    ;(println "comparing-l " left " and " right)
    (->> (zip left right)
         (reduce comparison-reducer :cont))))

(defn comparison-reducer
  [result [left right]]
  (let [s (println left right)
        r (if (cont? result)
            (cond
              (and (number? left) (number? right)) (do (println "num num") (compare-numbers left right))
              (and (seqable? left) (seqable? right)) (do (println "seq seq") (compare-lists left right))
              (and (seqable? left) (number? right)) (do (println "seq num") (compare-lists left [right]))
              (and (number? left) (seqable? right)) (do (println "num seq") (compare-lists [left] right))
              (and (cont? left) (some? right)) (do (println "nil some") true)
              (and (some? left) (cont? right)) (do (println "some nil") false))
            result)]
    (println "Comparing " left " and " right ": " r)
    r))

; part1
(let [pairs packet-pairs]
  (->> pairs
       (map (fn [[left right]] (compare-lists left right)))
       (zipmap (range 1 (inc (count pairs))))
       (filter second)
       (map first)
       (reduce +)))

; part2

(defn parse-part2
  [input]
  (->> (str/split-lines input)
       (filter (complement empty?))
       (map read-string)
       (concat [[[2]] [[6]]])))

(def sample-packets-part2 (parse-part2 sample-input))
(def packets-part2 (parse-part2 input))

(let [packets packets-part2]
  (->> packets
       (sort compare-lists)
       (zipmap (range 1 (inc (count packets))))
       (filter (fn [[k v]] (or (= [[2]] v) (= [[6]] v))))
       (map first)
       (reduce *)))
