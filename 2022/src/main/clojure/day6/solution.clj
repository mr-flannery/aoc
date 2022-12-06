(ns day6.solution
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.core.matrix :as m]))

(def input (slurp (io/resource "day6/input.txt")))

(def sample-input "mjqjpqmgbljsphdztnvjfqwrcgsmlb")

;; part 1
(->> input
     (partition 4 1)
     (map #(= 4 (count (set %))))
     (take-while false?)
     count
     (+ 4))

;; part 2
(->> input
     (partition 14 1)
     (map #(= 14 (count (set %))))
     (take-while false?)
     count
     (+ 14))