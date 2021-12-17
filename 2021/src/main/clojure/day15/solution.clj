(ns day15
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.data.priority-map :as d]))

(def example-input [[1 1 6 3 7 5 1 7 4 2]
                    [1 3 8 1 3 7 3 6 7 2]
                    [2 1 3 6 5 1 1 3 2 8]
                    [3 6 9 4 9 3 1 5 6 9]
                    [7 4 6 3 4 1 7 1 1 1]
                    [1 3 1 9 1 2 8 1 3 7]
                    [1 3 5 9 9 1 2 4 2 1]
                    [3 1 2 5 4 2 1 6 3 9]
                    [1 2 9 3 1 3 8 5 2 1]
                    [2 3 1 1 9 4 4 5 8 1]])

(def inc-risk (assoc (into {} (map vector (range 1 9) (range 2 10))) 9 1))

(defn inc-risk-m
  [m by]
  (map (fn [row] (map #((apply comp (repeat by inc-risk)) %) row)) m))

(defn part2 
  [m]
  (mapcat (fn [column] (apply map concat (map #(inc-risk-m m (+ % column)) (range 5)))) (range 5)))

(def example-input-2 (part2 example-input))

(def input-file (io/resource "day15/input.txt"))

(def real-input (for [line (str/split (slurp input-file) #"\n")]
                  (map #(Integer/parseInt %) (str/split line #""))))

(def real-input-2 (part2 real-input))

(defn neighbor-coords
  [m [x y]]
  (let [x-max (dec (count (first m)))
        y-max (dec (count m))
        x-neighbors (cond
                      (= x 0) [[(inc x) y]]
                      (= x x-max) [[(dec x) y]]
                      :else [[(dec x) y] [(inc x) y]])
        y-neighbors (cond
                      (= y 0) [[x (inc y)]]
                      (= y y-max) [[x (dec y)]]
                      :else [[x (dec y)] [x (inc y)]])]
    (concat x-neighbors y-neighbors)))

(defn get-point
  [m [x y]]
  (-> m
      (nth y)
      (nth x)))

(defn all-coords
  [m]
  (for [y (range (count m))
        x (range (count (first m)))]
    [x y]))

(defn p-queue
  [m]
  (into (d/priority-map) (map (fn [p] [p ##Inf]) (all-coords m))))

(defn dijkstra 
  [m]
  (loop [queue   (dissoc (p-queue m) [0 0])
         visited []]
  (let [[p path-risk] (let [[p path-risk] (peek queue)]
                        (if (= ##Inf path-risk)
                          [[0 0] 0]
                          [p path-risk]))]
    (if (empty? queue)
      visited
      (let [neighbors         (for [n (neighbor-coords m p)
                                    :when (contains? queue n)]
                                n)
            updated-neighbors (for [n neighbors
                                    :let [known-risk    (queue n)
                                          n-risk        (get-point m n)
                                          updated-risk  (min (+ path-risk n-risk) known-risk)]]
                                [n updated-risk])]
        (recur (dissoc (reduce (fn [q [p risk]] (assoc q p risk)) queue updated-neighbors) p) (conj visited [p path-risk]))
        )))))

;; part1 
(get (into {} (dijkstra real-input)) [99 99]) ;; 527

;; part2
(time (get (into {} (dijkstra real-input-2)) [499 499])) ;; 2887, ~20s
