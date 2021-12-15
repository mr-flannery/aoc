(ns day12
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def example-edges [["start" "A"]
                    ["start" "b"]
                    ["A" "c"]
                    ["A" "b"]
                    ["b" "d"]
                    ["A" "end"]
                    ["b" "end"]])

(def input-file (io/resource "day12/input.txt"))

(def real-edges (->> (str/split (slurp input-file) #"\n")
                     (map #(str/split % #"-"))))

(defn graph
  [edges]
  (reduce (fn [graph [src dest]] (if (contains? graph src)
                                   (update graph src #(concat % [dest]))
                                   (assoc graph src [dest]))) {} edges))

(defn undirected-graph
  [edges]
  (reduce (fn [graph [src dest]] (if (contains? graph src)
                                   (update graph src #(concat % [dest]))
                                   (assoc graph src [dest])))
          {}
          (concat edges (map (fn [[src dest]] [dest src]) edges))))

(undirected-graph real-edges)

(defn is-large-cavern?
  [s]
  (re-matches #"[A-Z]+" s))

(def is-small-cavern? (comp not is-large-cavern?))

(def billo-example [["start" "A"]
                    ["A" "b"]
                    ["A" "c"]
                    ["b" "c"]
                    ["A" "end"]])

;; mit echter Rekursion ist das ganze schon mal tausend mal einfacher
;; aber wie krieg ich das verdammte Ergebnis raus?
;; (ohne Seiteneffekte)
(def paths (atom []))
(defn dfs
  ([graph]
   (dfs graph "start" [] [] []))
  ([graph node visited current-path result-paths]
   (let [current-path (conj current-path node)]
     (if (= node "end")
      ;;  (conj result-paths current-path)
       current-path
       (for [neighbor (get graph node)]
         (if (not (.contains visited neighbor))
           (if (is-large-cavern? node)
             (dfs graph neighbor visited current-path result-paths)
             (dfs graph neighbor (conj visited node) current-path result-paths))))))))

;; (dfs (undirected-graph real-edges))
;; (count @paths)

;; okay wenn ichs jetzt endlich richtig verstanden habe müssen wir durch den graphen dfs-en
;; aber wir packen die jeweilige visited liste mit aufn Stack

(defn dfs-part1
  ([graph]
   (dfs-part1 graph "start"))
  ([graph node]
   (loop [stack (vector [node []])
          paths 0]
     (if (empty? stack)
       paths
       (let [[node visited] (peek stack)
             neighbors       (get graph node)
             new-small       (filter #(and
                                       (is-small-cavern? %)
                                       (not (.contains visited %)))
                                     neighbors)
             large           (filter #(is-large-cavern? %) neighbors)
             all-new         (concat new-small large)
             new-visited     (if (.contains visited node) visited (conj        visited node))
             all-new-s-elems (map (fn [v] [v, new-visited]) all-new)
             new-stack       (into (pop stack) all-new-s-elems)
             new-paths       (if (= node "end") (inc paths) paths)]
         (recur new-stack new-paths))))))

(dfs-part1 (undirected-graph billo-example))
(dfs-part1 (undirected-graph example-edges))
;; part1
(dfs-part1 (undirected-graph real-edges)) ;; 4241

;; (new-neigh-and-vis ["d" "a" "b" "c" "start"] ["start" "a" "b" "c"])

;; das is doch irgendwie auchn dead end...
(defn new-small-part2
  [neighbors visited]
  (let [new (remove (set visited) neighbors)
        relevant-visited (filter is-small-cavern? visited)]
    (if (> (count relevant-visited) (count (set relevant-visited)))
      new
      (conj new (first (remove (conj (set new) "start" "end") neighbors))))))

(let [visited []]
  (> (count visited) (count (set visited))))

(def ultra-billo [["start" "A"]
                  ["A" "b"]
                  ["A" "c"]
                  ["A" "end"]])

(undirected-graph ultra-billo)

(def x 5)

x

(defn function [])

(fn [])

(defn dfs-part2
  ([graph]
   (dfs-part2 graph "start"))
  ([graph node]
   (loop [stack (vector [node [] false])
          paths 0]
     (if (empty? stack)
       paths
       (let [[node visited twice] (peek stack)
             neighbors            (get graph node)
             new-small            (filter #(and
                                            (is-small-cavern? %)
                                            (not (.contains visited %)))
                                          neighbors)
             large                (filter #(is-large-cavern? %) neighbors)
             all-new              (concat new-small large)
             new-visited          (if (.contains visited node) visited (conj visited node))
             all-new-s-elems      (map (fn [v] [v new-visited twice]) all-new)
             extra-states         (if twice
                                    []
                                    (->> neighbors
                                         ;; ich könnte auch beim bauen das graphen schon Kanten nach start und end rausschmeißen
                                         (filter #(and (not (= "start" %)) (not (= "end" %))))
                                         (filter is-small-cavern?)
                                         (filter #(.contains visited %))
                                         (map (fn [v] [v, new-visited, true]))))
             new-stack            (into (into (pop stack) all-new-s-elems) extra-states)
             new-paths            (if (= node "end") (inc paths) paths)]
         (recur new-stack new-paths))))))

(dfs-part2 (undirected-graph ultra-billo))
(dfs-part2 (undirected-graph billo-example))
(dfs-part2 (undirected-graph example-edges))

(dfs-part2 (undirected-graph real-edges)) ;; 122134

;; go understand this solution https://github.com/fredoverflow/advent/blob/master/2021/aoc21l.clj
