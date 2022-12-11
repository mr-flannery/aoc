(ns day11.solution
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]])
  (:import (java.util HashSet)))

(def input (slurp (io/resource "day11/input.txt")))

(def sample-input "Monkey 0:\n  Starting items: 79, 98\n  Operation: new = old * 19\n  Test: divisible by 23\n    If true: throw to monkey 2\n    If false: throw to monkey 3\n\nMonkey 1:\n  Starting items: 54, 65, 75, 74\n  Operation: new = old + 6\n  Test: divisible by 19\n    If true: throw to monkey 2\n    If false: throw to monkey 0\n\nMonkey 2:\n  Starting items: 79, 60, 97\n  Operation: new = old * old\n  Test: divisible by 13\n    If true: throw to monkey 1\n    If false: throw to monkey 3\n\nMonkey 3:\n  Starting items: 74\n  Operation: new = old + 3\n  Test: divisible by 17\n    If true: throw to monkey 0\n    If false: throw to monkey 1")

(defn safe-parse-int
  [s]
  (try
    (Integer/parseInt s)
    (catch Exception e
      nil)))

(defn index-by
  [f col]
  (reduce (fn [m item] (assoc m (f item) item)) {} col))

(defn parse-monkeys
  [input]
  (let [monkeys (str/split input #"\n\n")]
    (->> (for [monkey monkeys]
           (let [[monkey-x starting-items operation test if-true if-false] (str/split-lines monkey)]
             {:monkey          (Integer/parseInt (re-find #"\d" monkey-x))
              :starting-items  (->> (re-seq #"\d+" starting-items) (map #(Integer/parseInt %)))
              :operation       (let [[_ mfn mop] (re-find #"new = old ([+\*]) (old|\d+)" operation)
                                     f  (if (= mfn "*") * +)
                                     op (safe-parse-int mop)]
                                 (if (nil? op)
                                   (fn [old] (f old old))
                                   (fn [old] (f old op))))
              :test            (let [op (Integer/parseInt (re-find #"\d+" test))]
                                 (fn [n] (= (mod n op) 0)))
              :if-true         (Integer/parseInt (re-find #"\d" if-true))
              :if-false        (Integer/parseInt (re-find #"\d" if-false))
              :inspect-counter 0}))
         (index-by :monkey))))

(defn parse-monkeys2
  [input]
  (let [monkeys (str/split input #"\n\n")]
    (->> (for [monkey monkeys]
           (let [[monkey-x starting-items operation test if-true if-false] (str/split-lines monkey)]
             {:monkey          (Integer/parseInt (re-find #"\d" monkey-x))
              :starting-items  (->> (re-seq #"\d+" starting-items) (map #(Integer/parseInt %)))
              :operation       (let [[_ mfn mop] (re-find #"new = old ([+\*]) (old|\d+)" operation)
                                     f  (if (= mfn "*") * +)
                                     op (safe-parse-int mop)]
                                 [mfn mop])
              :test            (let [op (Integer/parseInt (re-find #"\d+" test))]
                                 op)
              :if-true         (re-find #"\d" if-true)
              :if-false        (re-find #"\d" if-false)
              :inspect-counter 0}))
         (index-by :monkey))))

(def sample-monkeys (parse-monkeys sample-input))
(def monkeys (parse-monkeys input))

(defn play-round
  [monkeys]
  (let [a-monkeys (atom monkeys)]
    (loop [[idx & idxs] (keys monkeys)]
      (if (nil? idx)
        nil
        (do
          (let [{:keys [starting-items operation test if-true if-false]} (get @a-monkeys idx)]
            (println idx)
            (loop [[item & items] starting-items]
              (println item)
              (if (nil? item)
                nil
                (let [new-worry-level (-> item operation (#(/ % 3)) java.lang.Math/floor (#(.intValue %)))]
                  (do
                    (swap! a-monkeys update-in [idx :inspect-counter] inc)
                    (if (test new-worry-level)
                      (do
                        (println "throwing " new-worry-level " to monkey " if-true)
                        (swap! a-monkeys update-in [if-true :starting-items] #(conj % new-worry-level))
                        (recur items))
                      (do
                        (println "throwing " new-worry-level " to monkey " if-false)
                        (swap! a-monkeys update-in [if-false :starting-items] #(conj % new-worry-level))
                        (recur items)))))))
            (swap! a-monkeys assoc-in [idx :starting-items] []))
          (recur idxs))))
    ; why does the loop code correctly produce side-effects and the for code not?
    ;(for [idx (keys monkeys)] ; das kann ja nicht funktionieren! ich hole mir ja immer den state vom Anfang!
    ;  ;(println idx)
    ;  (let [{:keys [starting-items operation test if-true if-false]} (get @a-monkeys idx)]
    ;    (println idx)
    ;    (loop [[item & items] starting-items]
    ;      ;(println item)
    ;      (if (nil? item)
    ;        nil
    ;        (let [new-worry-level (-> item operation (#(/ % 3)) java.lang.Math/floor (#(.intValue %)))]
    ;          (if (test new-worry-level)
    ;            (do
    ;              (println "throwing " new-worry-level " to monkey " if-true)
    ;              (swap! a-monkeys update-in [if-true :starting-items] #(conj % new-worry-level))
    ;              (recur items))
    ;            (do
    ;              (println "throwing " new-worry-level " to monkey " if-false)
    ;              (swap! a-monkeys update-in [if-false :starting-items] #(conj % new-worry-level))
    ;              (recur items))))))
    ;    (swap! a-monkeys assoc-in [idx :starting-items] []))
    ;  )
    @a-monkeys
    ))

(defn throw-item
  [monkeys item from to]
  (-> monkeys
      (update-in [to :starting-items] #(conj % item))
      (update-in [from :starting-items] rest)))

(defn inc-inspection-counter
  [monkeys idx]
  (update-in monkeys [idx :inspect-counter] inc))

(defn play-turn-pure
  [monkeys idx]
  (let [{:keys [starting-items operation test if-true if-false]} (get monkeys idx)]
    (loop [monkeys monkeys
           [item & items] starting-items]
      (if (nil? item)
        monkeys
        (let [new-worry-level (-> item operation (#(/ % 3)) java.lang.Math/floor (#(.intValue %)))]
          (println item)
          (if (test new-worry-level)
            (do
              (println "throwing " new-worry-level " to monkey " if-true)
              (recur (-> monkeys 
                         (throw-item item idx if-true) 
                         (inc-inspection-counter idx)) 
                     items))
            (do
              (println "throwing " new-worry-level " to monkey " if-false)
              (recur (-> monkeys 
                         (throw-item item idx if-false) 
                         (inc-inspection-counter idx)) 
                     items))))))))

(defn play-round-pure
  [monkeys]
  (loop [monkeys monkeys
         [idx & idxs] (keys monkeys)]
    (if (nil? idx)
      monkeys
      (do
        (println idx)
        (recur (play-turn-pure monkeys idx) idxs)))))

(defn play-n-rounds
  [monkeys n]
  (loop [monkeys monkeys
         [round & rounds] (range 0 n)]
    (if (nil? round)
      monkeys
      (recur (play-round-pure monkeys) rounds))))

(let [monkeys (play-n-rounds monkeys 20)]
  (->> (vals monkeys)
       (map :inspect-counter)
       (sort-by -)
       (take 2)
       (apply *))
  ;(->> monkeys
  ;     vals
  ;     (map :starting-items)
  ;     (map count)
  ;     (reduce +))
  )
