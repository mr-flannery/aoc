(ns day8.solution
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]))

(def input (slurp (io/resource "day8/input.txt")))

(def tree-map (->> (str/split-lines input)
                   (mapv #(str/split % #""))
                   (mapv (fn [chars] (mapv #(Integer/parseInt %) chars)))))

(def sample-input [[3 0 3 7 3]
                   [2 5 5 1 2]
                   [6 5 3 3 2]
                   [3 3 5 4 9]
                   [3 5 3 9 0]])

(defn and-fn
  [a b]
  (and a b))

(defn is-visible?
  [m y x]
  (let [tree (get-in m [y x])
        top-y (for [y1 (range 0 y)]
                (get-in m [y1 x]))
        bottom-y (for [y1 (range (inc y) (count m))]
                   (get-in m [y1 x]))
        top-x (for [x1 (range 0 x)]
                (get-in m [y x1]))
        bottom-x (for [x1 (range (inc x) (count m))]
                   (get-in m [y x1]))]
    (or
      (reduce and-fn (map #(> tree %) top-y))
      (reduce and-fn (map #(> tree %) bottom-y))
      (reduce and-fn (map #(> tree %) top-x))
      (reduce and-fn (map #(> tree %) bottom-x)))))

(defn count-visible-trees
  [m]
  (+
    (->> (for [x (range 1 (dec (count m)))
               y (range 1 (dec (count (first m))))]
           (is-visible? m y x))
         (filter identity)
         count)
    (+ (* 2 (count m)) (* 2 (- (count m) 2)))
    ))

; part 1
(count-visible-trees tree-map)

(defn viewing-distance
  [tree trees]
  (let [smaller-trees (take-while #(< % tree) trees)]
    (if (= (count smaller-trees) (count trees))
      (count smaller-trees)
      (inc (count smaller-trees)))))

(defn scenic-factor
  [m y x]
  (let [tree (get-in m [y x])
        top-y (for [y1 (range 0 y)]
                (get-in m [y1 x]))
        bottom-y (for [y1 (range (inc y) (count m))]
                   (get-in m [y1 x]))
        top-x (for [x1 (range 0 x)]
                (get-in m [y x1]))
        bottom-x (for [x1 (range (inc x) (count m))]
                   (get-in m [y x1]))]
    (*
      (viewing-distance tree (reverse top-y))
      (viewing-distance tree bottom-y)
      (viewing-distance tree (reverse top-x))
      (viewing-distance tree bottom-x))))

(let [m tree-map]
  (apply max (for [x (range 1 (dec (count m)))
                   y (range 1 (dec (count (first m))))]
               (scenic-factor m y x))))
