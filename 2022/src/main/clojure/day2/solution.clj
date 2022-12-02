(ns day2.solution
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def sample-input [["A" "Y"]
                   ["B" "X"]
                   ["C" "Z"]])

(def score {"A" 1
            "B" 2
            "C" 3
            "X" 1
            "Y" 2
            "Z" 3})

(def winners [["A" "Y"] ["B" "Z"] ["C" "X"]])
(def draws [["A" "X"] ["B" "Y"] ["C" "Z"]])

(defn round-score
  [[opp me]]
  (+
    (score me)
    (cond
      (.contains winners [opp me]) 6
      (.contains draws [opp me]) 3
      :default 0)))

(def input-file (io/resource "day2/input.txt"))

(def input (->> (str/split (slurp input-file) #"\n")
                (map #(str/split % #" "))))

(defn part1
  [input]
  (->> input
       (map round-score)
       (reduce +)))

;; part 2
;; TODO: name
(def blergi {"A" {"X" "Z"
                  "Y" "X"
                  "Z" "Y"}
             "B" {"X" "X"
                  "Y" "Y"
                  "Z" "Z"}
             "C" {"X" "Y"
                  "Y" "Z"
                  "Z" "X"}})

(defn remap-choice
  [[opp me]]
  [opp (-> blergi (get opp) (get me))])

(defn part2
  [input]
  (part1 (map remap-choice input)))

(defn -main
  []
  (println "Part 1")
  (println (part1 input))
  (println "Part 2")
  (println (part2 input)))

(-main)
