(ns day9.solution
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]])
  (:import (java.util HashSet)))

(def input (slurp (io/resource "day9/input.txt")))

(def visited [[0 0]])

(def direction->fn
  {"R" (fn [[x y] steps] [(+ x steps) y])
   "L" (fn [[x y] steps] [(- x steps) y])
   "U" (fn [[x y] steps] [x (+ y steps)])
   "D" (fn [[x y] steps] [x (- y steps)])})

(def sample-instructions (->>
                           (str/split-lines "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2\n")
                           (map #(str/split % #" "))
                           (map (fn [[direction steps]]
                                  [(direction->fn direction)
                                   (Integer/parseInt steps)]))))

(def instructions (->>
                    (str/split-lines input)
                    (map #(str/split % #" "))
                    (map (fn [[direction steps]]
                           [(direction->fn direction)
                            (Integer/parseInt steps)]))))

(defn abs
  [a]
  (max a (- a)))

(defn distance
  [[x0 y0] [x1 y1]]
  (+ (abs (- x0 x1)) (abs (- y0 y1))))

(defn points-between
  [[x0 y0] [x1 y1]]
  (if (= -1 (compare [x0 y0] [x1 y1]))
    (if (= x0 x1)
      (for [y (range (dec y1) y0 -1)]
        [x0 y])
      (for [x (range (dec x1) x0 -1)]
        [x y0]))
    (reverse (points-between [x1 y1] [x0 y0]))))

;; ich hab den Fall vergessen 

(defn t-positions
  [h1 h2 t]
  (let [acc-fn (if (= (first h1) (first h2)) second first)
        path   (points-between h1 h2)]
    (condp = (distance h1 t)
      0 (points-between h1 h2)
      1 (cond
          ; h läuft über t 
          (.contains path t) (take-while #(> (distance % t) 0) path)
          ; h läuft neben t 
          (and
            (not (= (first h2) (first t)))
            (not (= (second h2) (second t)))) path
          ; h endet auf t
          (= h2 t) []
          ; h läuft entgegen t
          :default (concat path [h1]))

      2 (if (or (passes-y h1 h2 t) (passes-x h1 h2 t)) ; was ich rausfinden will ist läuft h neben t
          (take-while #(>= (distance % t) 2) path)
          (concat path [h1])))))

(defn passes-x
  [h1 h2 t]
  (or
    (->> [h1 t h2] (map first) (apply <=))
    (->> [h1 t h2] (map first) (apply >=))))
(defn passes-y
  [h1 h2 t]
  (or
    (->> [h1 t h2] (map second) (apply <=))
    (->> [h1 t h2] (map second) (apply >=))))

; part1
(->> (loop [h1h2    (->> (reduce (fn [points [i-fn steps]] (conj points (i-fn (last points) steps))) [[0 0]] instructions)
                         (partition 2 1)
                         ;(take 1000) 
                         )
            t       [0 0]
            visited #{[0 0]}]
       (if (empty? h1h2)
         visited
         (let [[[h1 h2] & bla] h1h2
               t-pos (t-positions h1 h2 t)
               new-t (or (first t-pos) t)]
           (println h1 h2 t)
           (println t-pos)
           (println new-t)
           (recur bla new-t (into visited t-pos)))))
     count
     )

; part 2
; the mental model for part1 will probably not work well for part2 :(
; if the rope moves by n < 10, only the first n knots will move
; it's probably easiest to simulate movements step by step
; and everytime the tail moves, just track it directly

(def rope [[0 0] [0 0] [0 0] [0 0] [0 0] [0 0] [0 0] [0 0] [0 0] [0 0]])

(defn moves
  [[x0 y0] [x1 y1]]
  (let [x-step     (if (< x0 x1) 1 -1)
        x-range-fn (if (< x0 x1) inc dec)
        y-step     (if (< y0 y1) 1 -1)
        y-range-fn (if (< y0 y1) inc dec)]
    (if (= x0 x1)
      (for [y (range (y-range-fn y0) (y-range-fn y1) y-step)]
        [x0 y])
      (for [x (range (x-range-fn x0) (x-range-fn x1) x-step)]
        [x y0]))))


(moves [8 0] [8 4])
(moves [4 0] [8 0])
(moves [8 0] [8 -4])
(moves [4 0] [0 0])

(defn new-t-pos
  [[hx hy] [tx ty]]
  (if (>= (distance [hx hy] [tx ty]) 2)
    (if (and (not (= hx tx)) (not (= hy ty)))
      ; move diagonally
      [((if (> hx tx) inc dec) tx) ((if (> hy ty) inc dec) ty)]
      ; move straight
      (first (points-between [hx hy] [tx ty])))
    [tx ty]))

(defn new-t-pos
  [[hx hy] [tx ty]]
  (if (and (not (= hx tx)) (not (= hy ty)))
    ; diagonal
    (if (>= (distance [hx hy] [tx ty]) 3)
      [((if (> hx tx) inc dec) tx) ((if (> hy ty) inc dec) ty)]
      [tx ty])
    ; straight
    (if (>= (distance [hx hy] [tx ty]) 2)
      (first (points-between [hx hy] [tx ty]))
      [tx ty])))

(new-t-pos [1 1] [0 0]) ; this is the problem
(new-t-pos [2 0] [0 0])
(new-t-pos [2 0] [4 0])
(new-t-pos [0 2] [0 4])
(new-t-pos [0 2] [0 0])
(new-t-pos [-2 1] [0 0])
(new-t-pos [2 1] [0 0])
(new-t-pos [-2 -1] [0 0])
(new-t-pos [2 -1] [0 0])

(defn move-rope-by-1
  [rope target]
  (loop [rope (into [] (concat [target] (rest rope)))
         idx  0]
    (if (= (inc idx) (count rope))
      rope
      (recur (assoc rope (inc idx) (new-t-pos (get rope idx) (get rope (inc idx)))) (inc idx)))))

(move-rope-by-1 [[9 0] [8 0] [7 0] [6 0] [5 0] [4 0] [3 0] [2 0] [1 0] [0 0]] [9 1])

(defn move-rope-by-1-mark-visited
  [^java.util.Set visited]
  (fn
    [rope target]
    (let [new-rope (move-rope-by-1 rope target)]
      (.add s (last new-rope))
      new-rope)))

(defn move-rope-mark-visited
  [^java.util.Set visited]
  (fn
    [rope target]
    (let [moves    (moves (first rope) target)
          reducer! (move-rope-by-1-mark-visited visited)]
      (reduce reducer! rope moves))))

(let [rope rope
      targets (reduce (fn [points [i-fn steps]] (conj points (i-fn (last points) steps))) [[0 0]] instructions)
      visited (let [s (new java.util.HashSet)]
                (.add s [0 0])
                s)
      reducer! (move-rope-mark-visited visited)]
  (reduce reducer! rope targets)
  (count visited))
