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
