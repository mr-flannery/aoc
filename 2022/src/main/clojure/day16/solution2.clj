(ns day16.solution2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]])
  (:import (java.util HashSet)))

(def input (slurp (io/resource "day16/input.txt")))

(def sample-input "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB\nValve BB has flow rate=13; tunnels lead to valves CC, AA\nValve CC has flow rate=2; tunnels lead to valves DD, BB\nValve DD has flow rate=20; tunnels lead to valves CC, AA, EE\nValve EE has flow rate=3; tunnels lead to valves FF, DD\nValve FF has flow rate=0; tunnels lead to valves EE, GG\nValve GG has flow rate=0; tunnels lead to valves FF, HH\nValve HH has flow rate=22; tunnel leads to valve GG\nValve II has flow rate=0; tunnels lead to valves AA, JJ\nValve JJ has flow rate=21; tunnel leads to valve II")

(defn parse
  [input]
  (->> (str/split-lines input)
       (map #(re-matches #"Valve (.{2}) has flow rate=(\d+); tunnels? leads? to valves? (.*)" %))
       (reduce (fn [graph [_ valve flow neighbors]] (assoc graph valve {:f (Integer/parseInt flow) :n (map str/trim (str/split neighbors #","))})) {})))

(def sample-graph (parse sample-input))
(def graph (parse input))

(defn dfs-cost
  [graph from to]
  (loop [[[valve cost] & others] [[from 0]]
         visited #{from}]
    (if (nil? valve)
      ##Inf
      (if (= valve to)
        cost
        (let [neighbors (->> (:n (get graph valve))
                             (filter #(not (.contains visited %)))
                             (map (fn [n] [n (inc cost)])))]
          (recur (concat neighbors others) (conj visited valve)))))))

; wtf? this should be 3 but returns 5
(dfs-cost sample-graph "BB" "JJ")

; is there a greedy bug in here?

; let's try brute forcing this bitch
(->> (let [max-round 30 
           start     "AA"
           graph     sample-graph
           results   (HashSet.)]
       (loop [[[round opened total-flow current calcs] & states] [[0 [] 0 start []]]]
         (if (nil? round)
           results
           (let [options     (->> graph
                                  (filter (fn [[v {:keys [f n]}]]
                                            (and
                                              (not (.contains opened v))
                                              (> f 0)
                                              (<= (inc (dfs-cost graph current v)) (- max-round round))))))
                 next-states (map (fn [[v {:keys [f n]}]]
                                    [(+ round (inc (dfs-cost graph current v)))
                                     (conj opened v)
                                     (+ total-flow (* f (- max-round round (inc (dfs-cost graph current v)))))
                                     v
                                     (conj calcs [f (- max-round round (inc (dfs-cost graph current v)))])])
                                  options)]
             (if (empty? next-states)
               (do
                 (.add results [round opened total-flow current calcs])
                 (recur states))
               (recur (into states next-states)))))))
     (sort-by (fn [[round opened total-flow current]] total-flow))
     ; I'm expecting 28*20 + 13*25 + 21*21 + 13*22 + 9*3 + 6*2
     ; but I'm getting [[20 28] [13 25] [21 19] [22 11] [3 7] [2 4]]
     (filter (fn [[round opened total-flow current]] (= opened ["DD" "BB" "JJ" "HH" "EE" "CC"])))
     )
