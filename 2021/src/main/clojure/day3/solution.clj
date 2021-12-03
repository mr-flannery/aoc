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
  (fn [freq] (if (comp-fn (get freq \0) (get freq \1))
               "0"
               "1")))

(def select-gamma-bit (selector-factory >))
(def select-epsilon-bit (selector-factory <))

(defn -main
  []
  (let [freqs (freqs-for-pos input)
        gamma-bits (map select-gamma-bit freqs)
        epsilon-bits (map select-epsilon-bit freqs)]
    (* (Integer/parseInt (apply str gamma-bits) 2) (Integer/parseInt (apply str epsilon-bits) 2))))

(println (-main))