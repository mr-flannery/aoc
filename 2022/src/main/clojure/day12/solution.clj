(ns day12.solution
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.data.priority-map :refer [priority-map]])
  (:import (java.util HashSet)))

(def input (slurp (io/resource "day12/input.txt")))

(def sample-input "aabqponm\nabcryxxl\naccszExk\nacctuvwj\nabdefghi")

; we model E as 26 so that it always has the highest value
; we model S as -1 because doesn't matter and I initially forgot about it
(def char->score (zipmap "SabcdefghijklmnopqrstuvwxyzE" (range -1 27)))

; do we always start top-left?
(def sample-map (->> (str/split-lines sample-input)
                     (mapv #(mapv char->score %))))

(def real-map (->> (str/split-lines input)
                   (mapv #(mapv char->score %))))

(defn find-starting-point
  [m]
  (->> (for [l (range 0 (count real-map))
             c (range 0 (count (first real-map)))]
         (if (= -1 (get-in m [l c])) [l c]))
       (filter identity)
       first))

(defn find-ending-point
  [m]
  (->> (for [l (range 0 (count real-map))
             c (range 0 (count (first real-map)))]
         (if (= 26 (get-in m [l c])) [l c]))
       (filter identity)
       first))

(defn in-bounds?
  [m [line column]]
  (and
    (<= 0 line (dec (count m)))
    (<= 0 column (dec (count (first m))))))

(defn neighbors
  [m [line column]]
  (->> [[(dec line) column] [(inc line) column] [line (dec column)] [line (inc column)]]
       (filter #(in-bounds? m %))))

(defn store-valid-path!
  [^java.util.Set s a-min-length path]
  (.add s path)
  (if (< (count path) @a-min-length)
    (swap! a-min-length (fn [_] (count path)))))

(defn init-p-queue
  [m start]
  (let [all-points (for [l (range 0 (count m))
                         c (range 0 (count (first m)))]
                     [l c])]
    (-> (apply priority-map (interleave all-points (repeat (count all-points) ##Inf)))
        (assoc start 0))))

(dissoc (init-p-queue sample-map [0 0]) [0 0])

; dijkstra
(defn cost-of-shortest-path
  [m starting-point]
  (let [end-point (find-ending-point m)
        p-queue   (init-p-queue m starting-point)]
    (loop [p-queue p-queue
           visited []]
      (let [[p cost] (first p-queue)]
        (if (= p end-point)
          cost
          (let [new-higher-neighbors (->> (neighbors m p)
                                          (filter #(<= (get-in m %) (inc (get-in m p))))
                                          (filter #(not (.contains visited %))))
                updated-p-queue      (reduce (fn [p-q n]
                                               (let [n-cost (get p-q n)]
                                                 (if (< cost n-cost)
                                                   (assoc p-q n (inc cost))
                                                   p-q))) p-queue new-higher-neighbors)]
            (recur (dissoc updated-p-queue p) (conj visited p))))))))

;part1
;(cost-of-shortest-path real-map (find-starting-point real-map))

(defn possible-start-points
  [m]
  (->> (for [l (range 0 (count m))
             c (range 0 (count (first m)))]
         [[l c] (get-in m [l c])])
       (filter #(= 0 (second %)))
       (map first))
  )

;part2
;(time (let [m real-map]
;        (->> (possible-start-points m)
;             (pmap (fn [start] [start (cost-of-shortest-path m start)])))))
