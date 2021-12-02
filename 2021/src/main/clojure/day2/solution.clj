(ns day2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input-file (io/resource "day2/input.txt"))

(def input
  (->> (str/split (slurp input-file) #"\n")
       (map #(str/split % #" "))
       (map (fn [[direction steps]] [direction (Integer/parseInt steps)]))))

(def example-input [["forward" 5]
                    ["down" 5]
                    ["forward" 8]
                    ["up" 3]
                    ["down" 8]
                    ["forward" 2]])

(def initial-position {:depth 0
                       :horizontal 0
                       :aim 0})

(defn update-position
  [current-position [direction steps]]
  (cond
    (= direction "forward") (update-in current-position [:horizontal] #(+ steps %))
    (= direction "down") (update-in current-position [:depth] #(+ steps %))
    (= direction "up") (update-in current-position [:depth] #(- % steps))))

(defn update-position-step2
  [current-position [direction steps]]
  (cond
    (= direction "forward") (-> current-position
                                (update-in [:horizontal] #(+ steps %))
                                (update-in [:depth] #(+ % (* steps (:aim current-position)))))
    (= direction "down") (update-in current-position [:aim] #(+ steps %))
    (= direction "up") (update-in current-position [:aim] #(- % steps))))

(defn -main
  []
  (let [{ :keys [depth horizontal]} (reduce update-position-step2 initial-position input)]
    (println (* depth horizontal))))

(-main)
