(ns day18.solution
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.set :refer [intersection union]])
  (:import (java.util HashSet)))

(def input (slurp (io/resource "day18/input.txt")))

(def sample-input "2,2,2\n1,2,2\n3,2,2\n2,1,2\n2,3,2\n2,2,1\n2,2,3\n2,2,4\n2,2,6\n1,2,5\n3,2,5\n2,1,5\n2,3,5")

(defn parse
  [input]
  (->> (str/split-lines input)
       (map #(str/split % #","))
       (map (fn [[x y z]] [(Integer/parseInt x) (Integer/parseInt y) (Integer/parseInt z)]))))

(def sample-cubes (parse sample-input))
(def real-cubes (parse input))

(defn abs
  [n]
  (max n (- n)))

(defn are-adjacent?
  [[x1 y1 z1] [x2 y2 z2]]
  (or
    (and (= [x1 y1] [x2 y2]) (= 1 (abs (- z1 z2))))
    (and (= [x1 z1] [x2 z2]) (= 1 (abs (- y1 y2))))
    (and (= [y1 z1] [y2 z2]) (= 1 (abs (- x1 x2))))))

(defn neighbors
  [[x y z]]
  [[(inc x) y z] [(dec x) y z] [x (inc y) z] [x (dec y) z] [x y (inc z)] [x y (dec z)]])

; part1
(defn part1
  [input]
  (loop [cubes #{}
         [cube & cs] input
         sides 0]
    (if (nil? cube)
      sides
      (recur (conj cubes cube) cs (+ sides (- 6 (->> (neighbors cube)
                                                     (map #(.contains cubes %))
                                                     (map #(if % 2 0))
                                                     (reduce +))))))))

;part2 
; I guess I could brute force it by taking the union of all neighbors of all cubes
; if one of those neighbors is NOT in cubes but all of *its* neighbors are, that's an air pocket
;(defn is-inside-neighbor?
;  [cubes [nx ny nz]]
;  (let [[min-x max-x] (let [xs (->> cubes
;                                    (filter (fn [[x y z]] (= [y z] [ny nz])))
;                                    (map first)
;                                    sort)]
;                        [(or (first xs) 1) (or (last xs) -1)])
;        [min-y max-y] (let [ys (->> cubes
;                                    (filter (fn [[x y z]] (= [x z] [nx nz])))
;                                    (map second)
;                                    sort)]
;                        [(or (first ys) 1) (or (last ys) -1)])
;        [min-z max-z] (let [zs (->> cubes
;                                    (filter (fn [[x y z]] (= [x y] [nx ny])))
;                                    (map last)
;                                    sort)]
;                        [(or (first zs) 1) (or (last zs)) -1])]
;    (and
;      (<= min-x nx max-x)
;      (<= min-y ny max-y)
;      (<= min-z nz max-z))))
;
;(comment
;  (time
;    (let [input       real-cubes
;          all-cubes   (part1 input)
;          air-pockets (->> input
;                           (map neighbors)
;                           (map #(into #{} %))
;                           (reduce union)
;                           (filter #(is-inside-neighbor? input %))
;                           (map (fn [c] [c (neighbors c)]))
;                           (filter (fn [[c n]]
;                                     (and
;                                       (not (.contains input c))
;                                       (->> n
;                                            (filter #(.contains input %))
;                                            count
;                                            (#(>= % 5))))))
;                           (map first)
;                           )
;          ]
;      (- all-cubes (part1 air-pockets))
;      )))
;
;; can I determine enclosing cube?
;; yes I can. What does that help me?
;
;; let's construct a real dumb example
;(def dumb-cubes (remove #{[1 1 1]} (for [x (range 0 3)
;                                         y (range 0 3)
;                                         z (range 0 3)]
;                                     [x y z])))
;
;(def dumb-cubes2 (remove #{[2 0 1] [2 1 1] [1 1 1]} (for [x (range 0 4)
;                                                          y (range 0 3)
;                                                          z (range 0 3)]
;                                                      [x y z])))
;
;; this approach is somewhat flawed and too slow
;;(def is-connected-to-outside?
;;  (memoize
;;    (fn
;;      [graph from perimeter]
;;      (loop [[node & nodes] [from]
;;             visited #{}]
;;        (cond
;;          (nil? node) false
;;          (.contains visited node) (recur nodes visited)
;;          (.contains perimeter node) true
;;          :default (recur (into nodes (get graph node)) (conj visited node)))))))
;
;; can I do a search/expand instead?
;; I start with a node
;; generate it's neighbors
;; filter out the ones that are part of the input set
;; check if its outside
;; if yes, return true
;; if not, recur with neighbors
;
;(defn is-inside-droplet?
;  [cubes-by-x-y cubes-by-x-z cubes-by-y-z [x y z]]
;  ;(println [x y z])
;  (if (and
;        (get-in cubes-by-x-y [x y])
;        (get-in cubes-by-x-z [x z])
;        (get-in cubes-by-y-z [y z]))
;    (let [xs (->> (get-in cubes-by-y-z [y z])
;                  (map first)
;                  sort)
;          ys (->> (get-in cubes-by-x-z [x z])
;                  (map second)
;                  sort)
;          zs (->> (get-in cubes-by-x-y [x y])
;                  (map #(get % 2))
;                  sort)]
;      ;(println xs ys zs)
;      (and
;        (<= (first xs) x (last xs)) ; is in the inner cube
;        (<= (first ys) y (last ys))
;        (<= (first zs) z (last zs))))
;    false))
;
;(defn is-connected-to-outside?
;  [cubes-by-x-y cubes-by-x-z cubes-by-y-z cubes node]
;  ;(println node)
;  (loop [[n & ns] [node]
;         visited #{}]
;    (cond
;      (nil? n) false
;      (.contains visited n) (recur ns visited)
;      (not (is-inside-droplet? cubes-by-x-y cubes-by-x-z cubes-by-y-z n)) true
;      :default (recur (into ns (->> (neighbors n)
;                                    (filter #(not (.contains cubes %))))) (conj visited n)))))
;
;(defn is-connected-to-outside?2
;  [min-x max-x min-y max-y min-z max-z cubes node]
;  ;(println node)
;  (loop [[n & ns] [node]
;         visited #{}]
;    ;(println n)
;    ;(Thread/sleep 100)
;    (let [[x y z] n]
;      (cond
;        (nil? n) false
;        (.contains visited n) (recur ns visited)
;        (or (< x min-x) (> x max-x)
;            (< y min-y) (> y max-y)
;            (< z min-z) (> z max-z)) true
;        :default (recur (into ns (->> (neighbors n)
;                                      (filter #(not (.contains cubes %))))) (conj visited n))))))
;
;; I'm not sure where my problem lies here, but let's ditch this approach for now
;; wait a second...
;(comment
;  (let [cubes        (into #{} real-cubes)
;        [min-x max-x] (let [xs (->> cubes
;                                    (map first)
;                                    sort)]
;                        [(first xs) (last xs)])
;        [min-y max-y] (let [ys (->> cubes
;                                    (map second)
;                                    sort)]
;                        [(first ys) (last ys)])
;        [min-z max-z] (let [zs (->> cubes
;                                    (map #(get % 2))
;                                    sort)]
;                        [(first zs) (last zs)])
;        cubes-by-x-y (->> cubes
;                          (group-by first)
;                          (map (fn [[x cubes]] [x (group-by second cubes)]))
;                          (into {}))
;        cubes-by-x-z (->> cubes
;                          (group-by first)
;                          (map (fn [[x cubes]] [x (group-by #(get % 2) cubes)]))
;                          (into {}))
;        cubes-by-y-z (->> cubes
;                          (group-by second)
;                          (map (fn [[x cubes]] [x (group-by #(get % 2) cubes)]))
;                          (into {}))
;        air-pockets  (->> (for [x (range min-x (inc max-x))
;                                y (range min-y (inc max-y))
;                                z (range min-z (inc max-z))
;                                ;:when (and
;                                ;        (is-inside-droplet? cubes-by-x-y cubes-by-x-z cubes-by-y-z [x y z])
;                                ;        (not (.contains cubes [x y z])))
;                                :when (is-inside-droplet? cubes-by-x-y cubes-by-x-z cubes-by-y-z [x y z])
;                                ]
;                            [x y z]))
;        ;pockets               (->> air-pockets
;        ;                           (filter (fn [[cube category]] (= category :pocket)))
;        ;                           (map first)
;        ;                           )
;        ;perimeter             (->> air-pockets
;        ;                           (filter (fn [[cube category]] (= category :perimeter)))
;        ;                           (map first)
;        ;                           )
;        ;pockets-and-perimeter (concat pockets perimeter)
;        ;pp-graph              (reduce (fn [m cube] (assoc m cube (filter #(are-adjacent? cube %) pockets-and-perimeter))) {} pockets-and-perimeter)
;        ;actual-pockets    (filter #(not (is-connected-to-outside?2 min-x max-x min-y max-y min-z max-z cubes %)) air-pockets)
;        ;actual-pockets    (filter #(is-connected-to-outside?2 min-x max-x min-y max-y min-z max-z cubes %) air-pockets)
;        ;bla               (println (count air-pockets))
;        ;bla               (println (count actual-pockets))
;        ;clustered-pockets (loop [[p & ps] actual-pockets
;        ;                         res []]
;        ;                    (if (nil? p)
;        ;                      res
;        ;                      (let [cluster (conj (->> ps
;        ;                                               (filter #(are-adjacent? p %))
;        ;                                               (into #{})) p)
;        ;                            rest    (remove cluster ps)]
;        ;                        (recur rest (conj res cluster))
;        ;                        ))
;        ;                    )
;        ;bla (println clustered-pockets)
;        ]
;    ;4261
;    (println (count cubes))
;    (println (type cubes))
;    (println (count air-pockets))
;    (println (type air-pockets))
;    ;(-
;    ;  (part1 cubes)
;    ;  (->> clustered-pockets
;    ;       (map part1)
;    ;       (reduce +)))
;
;    ))

;... and try the fred overflow approach
; border flood fill
; we want to construct a border of cubes around the droplet
; we start with the
(let [cubes      (into #{} real-cubes)
      minimum    (dec (transduce cat min Long/MAX_VALUE cubes))
      maximum    (inc (transduce cat max Long/MIN_VALUE cubes))
      in-bounds? (fn [[x y z]]
                   (and
                     (<= minimum x maximum)
                     (<= minimum y maximum)
                     (<= minimum z maximum)))]
  (loop [border  #{[minimum minimum minimum]}
         visited border
         surface 0]
    (if (empty? border)
      surface
      (let [border  (for [cube     border
                          neighbor (neighbors cube)
                          :when (and
                                  (not (visited neighbor))
                                  (in-bounds? neighbor))]
                      neighbor)
            surface (+ surface (count (filter cubes border))) ; this counts a cube that we reach in the droplet once for every side we reach it from
            border  (into #{} (filter (complement cubes) border)) ; this becomes the next set of cubes that we're looking from 
            ]
        (recur border (into visited border) surface)))))

; I do not get this
(transduce cat min Long/MAX_VALUE [[1 2 3]])
