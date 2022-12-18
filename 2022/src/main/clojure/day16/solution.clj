(ns day16.solution
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

;; greedy
; I stand at one point
; I need to find the valve that will produce the largest amount of gas for the given remainder of time

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

(dfs-cost sample-graph "AA" "JJ")
(dfs-cost sample-graph "AA" "HH")
(dfs-cost sample-graph "JJ" "DD")

; greedy one step doesn't even work on the example input...
(let [max-round 30
      start     "AA"
      graph     sample-graph]
  (loop [current       start
         current-round 1
         opened        #{}
         total-flow    0]
    (if (> current-round 30)
      total-flow
      (let [candidates (->> graph
                            (sort-by (comp :f second) >)
                            (filter #(not (.contains opened (first %)))))
            cands      (->> candidates
                            (map (fn [[valve {:keys [f]}]]
                                   [valve (* f (- max-round current-round (inc (dfs-cost graph current valve)))) (inc (dfs-cost graph current valve))]))
                            (sort-by second >)
                            )
            [go-to flow-inc cost] (first cands)]
        (pprint cands)
        (println go-to (+ current-round cost) (conj opened go-to) (+ total-flow flow-inc))
        (recur go-to (+ current-round cost) (conj opened go-to) (+ total-flow flow-inc))
        ))))

; maybe try calculating what the lowest total cost of traveling through the non-zero nodes is
; NOPE! That's too many permutations

; maybe try sorting by cost first instead of total-flow
(let [max-round 30
      start     "AA"
      graph     sample-graph]
  (loop [current       start
         current-round 1
         opened        #{}
         total-flow    0]
    (if (> current-round 30)
      total-flow
      (let [candidates (->> graph
                            (sort-by (comp :f second) >)
                            (filter #(not (.contains opened (first %)))))
            cands      (->> candidates
                            (map (fn [[valve {:keys [f]}]]
                                   [valve (* f (- max-round current-round (inc (dfs-cost graph current valve)))) (inc (dfs-cost graph current valve))]))
                            (sort-by #(get % 2))
                            (filter #(not= (second %) 0))
                            )]
        (pprint cands)
        (if-let [[go-to flow-inc cost] (first cands)]
          (do
            (println go-to (+ current-round cost) (conj opened go-to) (+ total-flow flow-inc))
            (recur go-to (+ current-round cost) (conj opened go-to) (+ total-flow flow-inc)))
          (recur current (inc current-round) opened total-flow))

        ))))
; also no

; p-queue

; judge a move by how many steps I would need for the next step?
; that's basically greedy 2step
(defn value
  [graph current valve current-round max-round]
  (* (:f (get graph valve)) (- max-round current-round (inc (dfs-cost graph current valve)))))

(defn magic-score
  "computes the sum of the remaining value divided by the the cost of moving there
  ideally, this should weigh of the value (pushing high-flow rate valves) vs cost of moving away from that node
  the higher the better(?)"
  [graph node opened current-round max-round]
  (->> graph
       (filter #(not (.contains opened (first %))))
       (filter #(not= node (first %)))
       (map (fn [[n {f :f}]] [n (/ (value graph node n current-round max-round) (dfs-cost graph node n))]))
       (map second)
       (reduce +)
       ))

(defn magic-magic-score
  "looks at all remaining nodes and ranks them by value * magic-score
  if this makes sense then I guess the magic-score is something like 'how good would it be to go there next' score..."
  [graph current opened current-round max-round]
  (->> graph
       (filter #(not= 0 (:f (second %))))
       (filter #(not (.contains opened (first %))))
       (map (fn [[v {:keys [f n]}]]
              [v (*
                   (value graph current v current-round max-round)
                   (magic-score graph v (conj opened current) current-round max-round))]))
       (sort-by second >)
       ))

(magic-score sample-graph "HH" #{} 1 30)
(magic-score sample-graph "DD" #{} 1 30)

; I don't even know what this shit is anymore, but it doesn't work on the sample and not remotely on the real input
(let [max-round 30
      start     "AA"
      graph     graph]
  (loop [current       start
         current-round 1
         opened        #{}
         total-flow    0]
    (if (> current-round 30)
      total-flow
      (let [candidates (->> graph
                            (sort-by (comp :f second) >)
                            (filter #(not (.contains opened (first %)))))
            cands      (->> candidates
                            (map (fn [[valve {:keys [f]}]]
                                   [valve (* f (- max-round current-round (inc (dfs-cost graph current valve)))) (inc (dfs-cost graph current valve))]))
                            (sort-by second >)
                            )
            [go-to flow-inc cost] (first cands)
            magic      (magic-magic-score graph current opened current-round max-round)
            [go-to] (first magic)
            ]
        ;(pprint cands)
        (if (some? go-to)
          (do
            (println go-to (+ current-round (inc (dfs-cost graph current go-to))) (conj opened go-to) (+ total-flow (value graph current go-to current-round max-round)))
            (recur go-to (+ current-round (inc (dfs-cost graph current go-to))) (conj opened go-to) (+ total-flow (value graph current go-to current-round max-round))))
          total-flow
          )

        ))))

; let's try to understand dynamic programming
;; chapter 1: memoization in Clojure
(defn fibonnaci
  [n]
  (condp = n
    0 0
    1 1
    (+ (fibonnaci (- n 1)) (fibonnaci (- n 2)))))

(time (fibonnaci 40)) ; "Elapsed time: 4261.692131 msecs"

(def fib-memo
  (memoize
    (fn [n]
      (condp = n
        0 0
        1 1
        (+ (fib-memo (- n 1)) (fib-memo (- n 2)))))))

(time (fib-memo 40)) ; "Elapsed time: 0.243163 msecs"

;; try lis (longest increasing subsequenece)
; (lis [3 1 8 2 5]) should return 3 for [1 2 5]
;; what is my base case?
;; for length 1 it's 1 (duh)

(defn remove-item-at-index
  [col n]
  (loop [[h & t] col
         res []
         i   0]
    (cond
      (= i (count col)) res
      (= i n) (recur t res (inc i))
      :else (recur t (concat res [h]) (inc i)))))

(defn all-subseqs
  [s]
  (for [i (range 0 (count s))]
    (remove-item-at-index s i)))

(all-subseqs [1 2 3])

;(def lis
;  (memoize
;    (fn
;      [sequence]
;      (if (= (count sequence) 1)
;        1
;        (->> (for [subs (all-subseqs sequence)]
;               (if (< (first sequence) (first subs))
;                 (inc (lis subs))
;                 (lis subs)))
;             (apply max))))))

(defn slice
  [from to col]
  (->> col (drop 1) (take (- to from))))

(def lis
  (memoize
    (fn
      [s]
      (if (= (count s) 1)
        1
        (apply max (for [i (range 1 (count s))]
                     (if (< (first s) (nth s i))
                       (inc (lis (slice i (count s) s)))
                       (lis (slice i (count s) s)))))))))

(defn lis2
  [s]
  (if (= (count s) 1)
    1
    (apply max (for [i (range 1 (count s))]
                 (if (< (first s) (nth s i))
                   (inc (lis2 (slice i (count s) s)))
                   (lis2 (slice i (count s) s)))))))

(lis [3 4])

(defn random-int
  [maximum]
  (-> (Math/random) (* maximum) inc (#(.intValue %))))

(time (lis (for [i (range 20)] (random-int 10000))))
(time (lis2 (for [i (range 24)] (random-int 10000))))

(time (lis [3 1 8 2 5 3 4]))
;; chapter2: find a sub problem
; can I do something like (max (prob steps-1) (prob steps))?
