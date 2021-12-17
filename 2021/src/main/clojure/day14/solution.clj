(ns day14
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def example-template "NNCB")

(def example-rules {"BB" "N"
                    "BC" "B"
                    "BH" "H"
                    "BN" "B"
                    "CB" "H"
                    "CC" "N"
                    "CH" "B"
                    "CN" "C"
                    "HB" "C"
                    "HC" "B"
                    "HH" "N"
                    "HN" "C"
                    "NB" "B"
                    "NC" "B"
                    "NH" "C"
                    "NN" "C"})

(def example-rules2 (into {} (for [[[a c] b] example-rules]
  [(str a c) [(str a b) (str b c)]])))


(def input-file (io/resource "day14/input.txt"))

(def real-template (first (str/split (slurp input-file) #"\n")))

(def real-rules (let [[_ _ & lines] (str/split (slurp input-file) #"\n")]
                  (->> lines
                       (map #(str/split % #" -> "))
                       (into {}))))

(def real-rules2 (into {} (for [[[a c] b] real-rules]
                               [(str a c) [(str a b) (str b c)]])))

;; of course, this approach does not scale for step2
(defn step
  [polymer rules]
  (let [pairs    (->>
                  (partition 2 1 polymer)
                  (map #(apply str %)))
        inserts  (map rules pairs)
        triplets (->> (map vector pairs inserts)
                      (map (fn [[[first second] insert]] (str first insert second))))]
    (apply str
           (flatten
            (concat
             (map #(take 2 %) triplets)
             [(last (last triplets))])))))

(defn expand
  [template rules steps]
  (reduce (fn [template _] (step template rules)) template (range steps)))

;; part1
(let [template    real-template
      rules       real-rules
      steps       10
      pairs       (partition 2 template)
      substrings  (pmap #(expand % rules steps) pairs)
      frequencies (sort-by second (frequencies (expand template rules steps)))]
  (- (second (last frequencies)) (second (first frequencies)))
  frequencies) ;; 3048

;; part2
;; danke Fred
(defn index-initial-template
  [template]
  (merge-with +
              (frequencies template)
              (frequencies (map #(apply str %) (partition 2 1 template)))))

(defn step
  [rules indexed-template]
  (apply merge-with +
         indexed-template
         (for [[pair count] indexed-template
               :when (string? pair)
               :let [[left right] (get rules pair)
                     new-char     (first right)]]
           (merge-with + {pair (- count)} {left count} {right count} {new-char count}))))

(step example-rules2 (step example-rules2 (index-initial-template example-template)))
(def step-w-rules (partial step example-rules2))

((take 10 (iterate step-w-rules (index-initial-template example-template))))

(let [template   real-template
      step       (partial step real-rules2)
      num-steps  40
      initial    (index-initial-template template)
      freqs      (->> initial
                      (iterate step)
                      (drop num-steps)
                      first
                      (filter (fn [[k v]] (char? k))))
      min        (apply min (vals freqs))
      max        (apply max (vals freqs))]
  (- max min)) ;; 3288891573057

