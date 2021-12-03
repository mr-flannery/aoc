(ns day3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def example-input ["00100"
                    "11110"
                    "10110"
                    "10111"
                    "10101"
                    "01111"
                    "00111"
                    "11100"
                    "10000"
                    "11001"
                    "00010"
                    "01010"])

(def input-file (io/resource "day3/input.txt"))

(def input (str/split (slurp input-file) #"\n"))

;; schlau 3000
(defn transpose [m]
  (apply mapv vector m))
;; is equivalent to (mapv vector ...m)
;; apply is similar to the spread operator in JS from a use case perspective
;; map (and mapv) can operate on multiple collections at the same time
;; assuming f is an fn that takes an arbitrary number of args
;; works: (map + [1 2 3] [1 2 3])
;; compiler error: (map inc [1 2 3] [1 2 3])

(defn freqs-for-pos
  [strings]
  (->> strings
       transpose
       (map frequencies)))

(defn selector-factory
  [comp-fn]
  (fn [freq] (if (comp-fn (get freq \1) (get freq \0))
               "1"
               "0")))

(def select-gamma-bit (selector-factory >))
(def select-epsilon-bit (selector-factory <))

(defn to-int
  [bits]
  (Integer/parseInt (apply str bits) 2))

(defn part1
  [input]
  (let [freqs (freqs-for-pos input)
        gamma-bits (map select-gamma-bit freqs)
        epsilon-bits (map select-epsilon-bit freqs)]
    (* (to-int gamma-bits) (to-int epsilon-bits))))

(def select-o2-bit (selector-factory >=))
(def select-co2-bit (selector-factory <))

;; this function looks waaay to complex
(defn select-bits
  [input select-fn]
  (let [i-max (dec (count (first input)))]
    (loop [input input
           result ""
           i 0]
      (let [freq (frequencies (nth (transpose input) i))
            next-bit (select-fn freq)
            mask (str result next-bit)
            candidates (filter #(str/starts-with? % mask) input)]
        (cond
          (= i i-max) mask
          (= (count candidates) 1) (first candidates)
          :else (recur candidates mask (inc i)))))))

;; this is the refactored version of part2
;; not necessarily less code, but somewhat simpler none the less
(defn select-bits-reducer
  [selector-fn]
  (fn
    [{:keys [candidates result]} index]
    (if (= 1 (count candidates))
      {:candidates candidates :result result}
      (let [next-bit (selector-fn (frequencies (nth (transpose candidates) index)))
            next-result (str result next-bit)
            next-candidates (filter #(str/starts-with? % next-result) candidates)]
        {:candidates next-candidates :result next-result}))))

(defn select-bits2
  [input selector-fn]
  (first (:candidates (reduce
                       (select-bits-reducer selector-fn)
                       {:candidates input :result ""}
                       (range (count (first input)))))))

(defn part2
  [input]
  (let [o2-bits (select-bits2 input select-o2-bit)
        co2-bits (select-bits2 input select-co2-bit)]
    (* (to-int o2-bits) (to-int co2-bits))))

(defn -main
  []
  (println (part1 input)) ;; 3985686
  (println (part2 input))) ;; 2555739

(-main)
