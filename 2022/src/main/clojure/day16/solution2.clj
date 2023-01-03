(ns day16.solution2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.data.priority-map :refer [priority-map]])
  (:import (java.util HashSet HashMap)))

(def input (slurp (io/resource "day16/input.txt")))

(def sample-input "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB\nValve BB has flow rate=13; tunnels lead to valves CC, AA\nValve CC has flow rate=2; tunnels lead to valves DD, BB\nValve DD has flow rate=20; tunnels lead to valves CC, AA, EE\nValve EE has flow rate=3; tunnels lead to valves FF, DD\nValve FF has flow rate=0; tunnels lead to valves EE, GG\nValve GG has flow rate=0; tunnels lead to valves FF, HH\nValve HH has flow rate=22; tunnel leads to valve GG\nValve II has flow rate=0; tunnels lead to valves AA, JJ\nValve JJ has flow rate=21; tunnel leads to valve II")

(defn parse
  [input]
  (->> (str/split-lines input)
       (map #(re-matches #"Valve (.{2}) has flow rate=(\d+); tunnels? leads? to valves? (.*)" %))
       (reduce (fn [graph [_ valve flow neighbors]] (assoc graph valve {:f (Integer/parseInt flow) :n (map str/trim (str/split neighbors #","))})) {})))

(def sample-graph (parse sample-input))
(def graph (parse input))

(defn init-p-queue
  [graph from]
  (assoc
    (apply priority-map (interleave (keys graph) (repeat (count graph) ##Inf)))
    from
    0))

(def cost-of-shortest-path
  (memoize (fn
             [graph from to]
             (loop [p-queue (init-p-queue graph from)
                    visited []]
               (let [[p cost] (first p-queue)]
                 (if (= p to)
                   cost
                   (let [neighbors       (->> (:n (get graph p))
                                              (filter #(not (.contains visited %))))
                         updated-p-queue (reduce (fn [p-q n]
                                                   (let [n-cost (get p-q n)]
                                                     (if (< cost n-cost)
                                                       (assoc p-q n (inc cost))
                                                       p-q))) p-queue neighbors)]
                     (recur (dissoc updated-p-queue p) (conj visited p)))))))))


; let's try brute forcing this
; okay I might need somehing like a p-queue here...
(defn state-p-queue
  [state]
  (priority-map state 0))

(def next-options
  (memoize
    (fn next-options
      [graph opened current max-round round]
      (->> graph
           (filter (fn [[v {:keys [f n]}]]
                     (and
                       (not (.contains opened v))
                       (> f 0)
                       (<= (inc (cost-of-shortest-path graph current v)) (- max-round round)))))
           (sort-by (fn [[v {:keys [f n]}]] f) >)))))

;part 1
(comment
  (time
    (->> (let [max-round 30
               start     "AA"
               graph     graph
               results   (HashSet.)]
           (loop [p-queue (state-p-queue [0 [] 0 start []])]
             (let [[[round opened total-flow current calcs]] (first p-queue)]
               (if (nil? round)
                 results
                 (let [options      (next-options graph opened current max-round round)
                       next-states  (map (fn [[v {:keys [f n]}]]
                                           [(+ round (inc (cost-of-shortest-path graph current v)))
                                            (conj opened v)
                                            (+ total-flow (* f (- max-round round (inc (cost-of-shortest-path graph current v)))))
                                            v
                                            (conj calcs [f (- max-round round (inc (cost-of-shortest-path graph current v)))])])
                                         options)

                       next-p-queue (dissoc
                                      (reduce
                                        (fn [p-q [round opened total-flow current calcs]]
                                          (assoc p-q [round opened total-flow current calcs] total-flow))
                                        p-queue next-states)
                                      [round opened total-flow current calcs])
                       ]

                   (if (empty? next-states)
                     (do
                       (.add results [round opened total-flow current calcs])
                       (recur next-p-queue))
                     (recur next-p-queue)))))))
         (sort-by (fn [[round opened total-flow current]] total-flow))
         last
         ;(filter (fn [[round opened total-flow current]] (= opened ["DD" "BB" "JJ" "HH" "EE" "CC"])))
         )))

(defn option-pairs-for-both
  [[round pos1 next-turn1 next-valve1 pos2 next-turn2 next-valve2 opened opened-in-order flow-rate total-flow] max-round]
  (let [
        ;options1     (next-options graph opened pos1 max-round round)
        ;options2     (for [option1 options1]
        ;               (next-options graph (conj opened (first option1)) pos2 max-round round))
        ;option-pairs (mapcat #(zip (repeat (count options1) (nth options1 %)) (nth options2 %)) (range 0 (count options1)))
        ;new-states   (->> option-pairs
        ;                  (map (fn choose-both [[[v1 {f1 :f n1 :n}] [v2 {f2 :f n2 :n}]]]
        ;                         [(inc round)
        ;                          v1 ; when do I update the current position? pos1
        ;                          (+ next-turn1 (inc (cost-of-shortest-path graph pos1 v1)))
        ;                          v1
        ;                          v2 ; when do I update the current position? pos2
        ;                          (+ next-turn2 (inc (cost-of-shortest-path graph pos2 v2)))
        ;                          v2
        ;                          (into opened [v1 v2])
        ;                          (into opened-in-order [[v1 (+ next-turn1 (inc (cost-of-shortest-path graph pos1 v1)))] [v2 (+ next-turn2 (inc (cost-of-shortest-path graph pos2 v2)))]])
        ;                          flow-rate
        ;                          total-flow
        ;                          ]
        ;                         )))
        ]

    ))

; ===
; TODO: think hard whether part2 can somehow be solved in terms of part1
; this is getting haaard....
(comment
  (time
    (->> (let [max-round      26
               start          "AA"
               graph          graph
               max-flow-rate  (->> graph (map (fn [[k v]] (:f v))) (reduce +))
               results        (HashSet.)
               max-total-flow (atom 0)]
           (println max-flow-rate)
           (loop [[[round pos1 next-turn1 next-valve1 pos2 next-turn2 next-valve2 opened opened-in-order flow-rate total-flow] & states] [[0 start 0 nil start 0 nil #{} [] 0 0]]]

             ;(println (count states))
             (if (nil? round)
               results
               (if (= round max-round)
                 (do
                   (.add results [round pos1 next-turn1 next-valve1 pos2 next-turn2 next-valve2 opened opened-in-order flow-rate total-flow])
                   (if (> total-flow @max-total-flow)
                     (reset! max-total-flow total-flow))
                   (recur states))
                 (let [[round pos1 next-turn1 next-valve1 pos2 next-turn2 next-valve2 opened opened-in-order flow-rate total-flow] (let [new-flow-1 (if (= round next-turn1)
                                                                                                                                                      (if (nil? next-valve1)
                                                                                                                                                        0
                                                                                                                                                        (:f (get graph next-valve1)))
                                                                                                                                                      0)
                                                                                                                                         new-flow-2 (if (= round next-turn2)
                                                                                                                                                      (if (nil? next-valve2)
                                                                                                                                                        0
                                                                                                                                                        (:f (get graph next-valve2)))
                                                                                                                                                      0)
                                                                                                                                         flow-rate  (+ flow-rate new-flow-1 new-flow-2)]
                                                                                                                                     [round pos1 next-turn1 next-valve1 pos2 next-turn2 next-valve2 opened opened-in-order flow-rate (+ total-flow flow-rate)])
                       new-states (cond
                                    ; TODO: there might be an edge case lurking here where I need to choose two but only have one more option
                                    (= round next-turn1 next-turn2) (for [option1 (next-options graph opened pos1 max-round round)
                                                                          option2 (next-options graph (conj opened (first option1)) pos2 max-round round)
                                                                          :let [[v1 {f1 :f n1 :n}] option1
                                                                                [v2 {f2 :f n2 :n}] option2]]
                                                                      [(inc round)
                                                                       v1 ; when do I update the current position? pos1
                                                                       (+ next-turn1 (inc (cost-of-shortest-path graph pos1 v1)))
                                                                       v1
                                                                       v2 ; when do I update the current position? pos2
                                                                       (+ next-turn2 (inc (cost-of-shortest-path graph pos2 v2)))
                                                                       v2
                                                                       (conj opened v1 v2)
                                                                       (into opened-in-order [[v1 (+ next-turn1 (inc (cost-of-shortest-path graph pos1 v1)))] [v2 (+ next-turn2 (inc (cost-of-shortest-path graph pos2 v2)))]])
                                                                       flow-rate
                                                                       total-flow
                                                                       ])
                                    (= round next-turn1) (for [[v1 {f1 :f n1 :n}] (next-options graph opened pos1 max-round round)]
                                                           [(inc round)
                                                            v1 ; when do I update the current position? pos1
                                                            (+ next-turn1 (inc (cost-of-shortest-path graph pos1 v1)))
                                                            v1
                                                            pos2 ; when do I update the current position? pos2
                                                            next-turn2
                                                            next-valve2
                                                            (conj opened v1)
                                                            (into opened-in-order [[v1 (+ next-turn1 (inc (cost-of-shortest-path graph pos1 v1)))]])
                                                            flow-rate
                                                            total-flow
                                                            ])
                                    (= round next-turn2) (for [[v2 {f2 :f n2 :n}] (next-options graph opened pos2 max-round round)]
                                                           [(inc round)
                                                            pos1 ; when do I update the current position? pos1
                                                            next-turn1
                                                            next-valve1
                                                            v2 ; when do I update the current position? pos2
                                                            (+ next-turn2 (inc (cost-of-shortest-path graph pos2 v2)))
                                                            v2
                                                            (conj opened v2)
                                                            (into opened-in-order [[v2 (+ next-turn2 (inc (cost-of-shortest-path graph pos2 v2)))]])
                                                            flow-rate
                                                            total-flow
                                                            ])
                                    (= flow-rate max-flow-rate) (do
                                                                  ;(println flow-rate)
                                                                  [[max-round pos1 next-turn1 next-valve1 pos2 next-turn2 next-valve2 opened opened-in-order flow-rate (+ total-flow (* flow-rate (- (dec max-round) round)))]])
                                    :default (do [[(inc round) pos1 next-turn1 next-valve1 pos2 next-turn2 next-valve2 opened opened-in-order flow-rate total-flow]]))
                       new-states (if (empty? new-states)
                                    [[(inc round) pos1 next-turn1 next-valve1 pos2 next-turn2 next-valve2 opened opened-in-order flow-rate total-flow]]
                                    new-states)
                       ]
                   (if (> @max-total-flow 0)
                     (let [total-flow-diff             (- @max-total-flow total-flow)
                           req-rounds-at-max-flow-rate (-> total-flow-diff
                                                           (/ max-flow-rate)
                                                           Math/ceil
                                                           .intValue
                                                           )
                           next-possible-move          (max 0 (min (- round next-turn1) (- round next-turn2)))]
                       (if (< (- (dec max-round) round) req-rounds-at-max-flow-rate) ; better result is impossible
                         (recur states)
                         (if (and ; max-flow-rate has not been reached yet but is impossible to reach in time under any condition
                               (not= flow-rate max-flow-rate)
                               (< (- (dec max-round) round next-possible-move) req-rounds-at-max-flow-rate))
                           (recur states)
                           (recur (into new-states states)))
                         )
                       )
                     (recur (into new-states states)))
                   )

                 ))))
         (sort-by last <)
         last
         ;sort 
         )))

(next-options sample-graph #{"AA" "BB" "CC" "DD" "EE" "FF" "GG" "HH"} "AA" 10 30)

(defn zip
  [& cols]
  (partition (count cols) (apply interleave cols)))

(let [vec1 [2 3 4 5 1]
      vec2 (repeat 5 [1 2 3 4 5])]
  (map #(zip (repeat 5 (nth vec1 %)) (nth vec2 %)) (range 0 5))
  )
(let [max-flow-at-round (HashMap.)]
  (< (or (get max-flow-at-round 0) 0) 0)
  )

(let [[[word] & ba] (into (state-p-queue "ass") [["bobs" -10]])]
  (println word)
  (println ba))

(map (fn [a b] [a b]))
