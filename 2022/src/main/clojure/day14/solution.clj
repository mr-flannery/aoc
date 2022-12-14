(ns day14.solution
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]])
  (:import (java.util HashSet)))

(def input (slurp (io/resource "day14/input.txt")))

(def sample-input "498,4 -> 498,6 -> 496,6\n503,4 -> 502,4 -> 502,9 -> 494,9")

(defn parse
  [input]
  (->> (str/split-lines input)
       (map #(str/split % #" -> "))
       (map (fn [points]
              (map (fn [point]
                     (let [[x y] (str/split point #",")]
                       [(Integer/parseInt x) (Integer/parseInt y)])) points)))
       (map #(partition 2 1 %))
       (map (fn [point-pairs]
              (map (fn [[p1 p2]]
                     (let [[x1 y1] p1
                           [x2 y2] p2]
                       (if (= x1 x2)
                         (if (< y1 y2)
                           (for [y (range y1 (inc y2))]
                             [x1 y])
                           (for [y (range y1 (dec y2) -1)]
                             [x1 y]))
                         (if (< x1 x2)
                           (for [x (range x1 (inc x2))]
                             [x y1])
                           (for [x (range x1 (dec x2) -1)]
                             [x y1])))))
                   point-pairs)))
       (map #(apply concat %))
       (reduce #(into %1 %2) #{})))

(def sample-rocks (parse sample-input))
(def rocks (parse input))

(defn is-free?
  [rocks sand field]
  (and (not (.contains rocks field)) (not (.contains sand field))))

(let [entry-point [500 0]
      rocks       rocks
      max-depth   (->> rocks (map second) (apply max))
      sand        (java.util.HashSet.)]
  (println max-depth)
  (loop [i :next]
    (if (= i :done)
      nil
      (recur (loop [[px py] entry-point]
               (let [[nx ny] [px (inc py)]]
                 (println [nx ny])
                 (cond
                   (= py max-depth) :done
                   (not (is-free? rocks sand [nx ny])) (cond
                                                         (is-free? rocks sand [(dec nx) ny]) (recur [(dec nx) ny])
                                                         (is-free? rocks sand [(inc nx) ny]) (recur [(inc nx) ny])
                                                         :default (do (.add sand [px py])))
                   :default (recur [nx ny])))))
      ))
  (println (sort sand))
  (println (count sand)))

