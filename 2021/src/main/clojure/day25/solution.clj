(ns day25
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def example-input ["v...>>.vv>"
                    ".vv>>.vv.."
                    ">>.>v>...v"
                    ">>v>>.>.v."
                    "v>v.vv.v.."
                    ">.>>..v..."
                    ".vv..>.>v."
                    "v.v..>>v.v"
                    "....v..v.>"])

(def real-input (str/split (slurp (io/resource "day25/input.txt")) #"\n"))

;; blerg
(def max-x 139)
(def max-y 137)

(defn next-x
  [n]
  (mod (inc n) max-x))

(defn next-y
  [n]
  (mod (inc n) max-y))

(defn index-area
  [input]
  (apply merge-with conj (for [y (range 0 (count input))
                               x (range 0 (count (first input)))]
                           (cond
                             (= \> (get-in input [y x])) {:east [[y x]]}
                             (= \v (get-in input [y x])) {:south [[y x]]}))))

(defn index-area2
  [input]
  (into {} (for [y (range 0 (count input))
                 x (range 0 (count (first input)))]
             (cond
               (= \> (get-in input [y x])) [[y x] :east]
               (= \v (get-in input [y x])) [[y x] :south]))))

(defn step
  [field]
  (let [current-easts  (->> field
                            (filter (fn [[k v]] (= v :east)))
                            (into {}))
        current-souths (->> field
                            (filter (fn [[k v]] (= v :south)))
                            (into {}))
        next-easts      (->> (for [[y x] (->> current-easts
                                              (map first))]
                               (if (not (contains? field [y (next-x x)]))
                                 [[y (next-x x)] :east]
                                 [[y x] :east]))
                             (filter some?))
        temp-field      (into {} (concat current-souths next-easts))
        next-souths     (->> (for [[y x] (->> current-souths
                                              (map first))]
                               (if (not (contains? temp-field [(next-y y) x]))
                                 [[(next-y y) x] :south]
                                 [[y x] :south]))
                             (filter some?))]
    (into {} (concat next-easts next-souths))))

(defn print-field
  [field]
  (let [dummy-field (into {} (for [y (range 0 max-y)
                                   x (range 0 max-x)]
                               [[y x] :empty]))]
    (->> (merge dummy-field field)
         sort
         (map (fn [[[y x] v]]
                (condp = v
                  :east  \>
                  :south \v
                  :empty \.)))
         (partition 10)
         (map #(apply str %))
         (#(doseq [line %] (println line))))))

(defn run-steps
  [field]
  (loop [field field
         steps 1]
    (let [next-field (step field)]
      (if (= field next-field)
        {:steps steps :field field}
        (recur next-field (inc steps))))))

(run-steps (index-area2 real-input))