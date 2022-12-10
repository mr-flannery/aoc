(ns day9.solution
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]))

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

(let [h2 [4 4]
      t  [3 0]] (drop-last (points-between [4 0] [4 4])))

(concat '([3 0]) [[2 0]])

(conj [[3 0]] [2 0])
(t-positions [2 0] [6 0] [3 0])
(t-positions [2 0] [6 0] [1 1])
(t-positions [4 4] [1 4] [4 3])
(t-positions [4 0] [4 4] [3 0])
(t-positions [1 0] [0 0] [0 0])

(let [h1   [2 2]
      h2   [-1 2]
      t    [1 1]
      ;acc-fn (if (= (first h1) (first h2)) second first)
      path (points-between h1 h2)]
  (println path)
  (if (or (passes-y h1 h2 t) (passes-x h1 h2 t)) ; was ich rausfinden will ist läuft h neben t
    (take-while #(>= (distance % t) 2) path)
    (concat path [h1])))

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

(passes-y [2 2] [2 0] [1 1]) ; true
(passes-y [2 0] [2 2] [1 1]) ; true
(passes-y [2 1] [2 2] [1 1]) ; true
(passes-y [2 2] [2 3] [1 1]) ; false
(passes-x [2 2] [0 2] [1 1]) ; true
(passes-x [0 2] [2 2] [1 1]) ; true
(passes-x [1 2] [2 2] [1 1]) ; true
(passes-x [2 2] [3 2] [1 1]) ; false

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
