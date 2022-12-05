(ns day5.solution
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.core.matrix :as m]))

(def sample-input "    [D]    \n[N] [C]    \n[Z] [M] [P]\n 1   2   3 \n\nmove 1 from 2 to 1\nmove 3 from 1 to 3\nmove 2 from 2 to 1\nmove 1 from 1 to 2")

(defn crates-into-seqs
  [input]
  (->> (for [line (str/split-lines (first (str/split input #"\n\n")))]
         (->> (partition-all 4 4 line)
              (map (fn [group] (apply str group)))
              (map (fn [str] (re-seq #"[A-Z]" str)))))
       (drop-last 1)))

(defn transpose
  [m]
  (apply mapv vector m))

(comment
  (transpose [[1 2] [3 4]])
  (mapv vector [1 2])                                       ; creates a vector of every element in the collection
                                                            ; => [[1] [2]]
  (mapv vector [1 3] [2 4])                                 ; map and mapv can operate on multiple vectors at once.
                                                            ; so it will create a vector from the first, second etc. elements from all the input cols
                                                            ; => [[1 2] [3 4]]
  (mapv vector [1 4] [2 5] [3 6 7])                         ; this only works for "well-formed" inputs, though
                                                            ; => [[1 2 3] [4 5 6]]
                                                            ; the 7 from the last col is being dropped, since mapv stops once the first collection is exhausted
                                                            ; a more robust implementation would require some padding, etc
  )

(defn parse-crates
  [input]
  ; TODO: transpose might be a problem
  (into [] (->> (m/transpose (crates-into-seqs input))
                (map #(filter some? %))
                (map flatten)
                (map #(into [] %)))))

(def sample-crates (parse-crates sample-input))

(defn parse-instructions
  [input]
  (->> (str/split-lines (second (str/split input #"\n\n")))
       (map #(re-matches #"move (\d+) from (\d+) to (\d+)" %))
       (map (fn [[_ num from to]]
              {:num (Integer/parseInt num) :from (dec (Integer/parseInt from)) :to (dec (Integer/parseInt to))}))))

(def sample-instructions (parse-instructions sample-input))

(def input-file (io/resource "day5/input.txt"))

(def crates (parse-crates (slurp input-file)))
(def instructions (parse-instructions (slurp input-file)))

(defn apply-instruction
  [crates instruction]
  (let [{:keys [num from to]} instruction
        new-from (into [] (drop num (get crates from)))
        move (reverse (take num (get crates from)))
        new-to (into [] (concat move (get crates to)))]
    (-> crates
        (assoc from new-from)
        (assoc to new-to))))

(defn apply-instruction-9001
  [crates instruction]
  (let [{:keys [num from to]} instruction
        new-from (into [] (drop num (get crates from)))
        move (take num (get crates from))
        new-to (into [] (concat move (get crates to)))]
    (-> crates
        (assoc from new-from)
        (assoc to new-to))))

(apply str (->> instructions
                (reduce apply-instruction crates)
                (map first)))

(apply str (->> instructions
                (reduce apply-instruction-9001 crates)
                (map first)))
