(ns day1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input-file (io/resource "day1/input.txt"))

(defn to-int-col
  [s]
  (map #(Integer/parseInt %) (str/split s #"\n")))


(defn num-increases
  [col]
  (->> col
     (partition 2 1)
     (filter (fn [[first second]] (> second first)))
     count))

(defn -main
  []
  (println (num-increases (to-int-col (slurp input-file)))))
