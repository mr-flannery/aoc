(ns day1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input-file (io/resource "day1/input.txt"))

(defn to-int-col
  [s]
  (map #(Integer/parseInt %) (str/split s #"\n")))

(def input (to-int-col (slurp input-file)))

(defn num-increases
  [col]
  (->> col
     (partition 2 1)
     (filter (fn [[first second]] (> second first)))
     count))

(defn three-measurement-window-sums
  [col]
  (->> col
       (partition 3 1)
       (map #(reduce + %))))

(defn -main
  []
  (println "Part 1")
  (println (num-increases input))
  (println "Part 2")
  (println (num-increases (three-measurement-window-sums input))))
