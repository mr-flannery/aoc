(ns day21.solution
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.set :refer [intersection union]])
  (:import (java.util HashSet)))

(def input (slurp (io/resource "day21/input.txt")))

(def sample-input "root: pppw + sjmn\ndbpl: 5\ncczh: sllz + lgvd\nzczc: 2\nptdq: humn - dvpt\ndvpt: 3\nlfqf: 4\nhumn: 5\nljgn: 2\nsjmn: drzm * dbpl\nsllz: 4\npppw: cczh / lfqf\nlgvd: ljgn * ptdq\ndrzm: hmdt - zczc\nhmdt: 32")

(defn parse
  [input]
  (->> (str/split-lines input)
       (map #(re-matches #"([a-z]{4}): (?:(\d+)|([a-z]{4}) ([+\-\*/]) ([a-z]{4}))" %))
       (map (fn [[_ node num op1 func op2]]
              [node (if (some? num)
                      (Integer/parseInt num)
                      {:op1 op1 :op2 op2 :fn (condp = func "+" + "-" - "*" * "/" /)})]))
       (into {})))

(def sample-graph (parse sample-input))
(def graph (parse input))

(defn compute
  [graph node]
  (let [v (get graph node)]
    (if (number? v)
      v
      ((get v :fn) (compute graph (get v :op1)) (compute graph (get v :op2))))))

;part1
(comment
  (time
    (let [graph graph]
      (compute graph "root")))
  )

;part2
; rhs is staying constant
; the _larger_ I set humn, the _smaller_ lhs gets
; so I could start searching by increasing in steps of 10000 or something, and as soon as the result shifts, I know that I need to start binary searching 
; edit: manual trying finds that increasing in orders of magnitude is a good idea
(defn test-humn
  [graph humn]
  (let [graph (-> (assoc graph "humn" humn) (assoc-in ["root" :fn] =))
        lhs   (get-in graph ["root" :op1])
        rhs   (get-in graph ["root" :op2])]
    (compare (compute graph lhs) (compute graph rhs))))

(defn dummy-test
  [n]
  (compare n 3456789))

(let [n1 175
      n2 1000]
  (/ (+ n2 n1) 2))

; let's do one linearly increasing loop...
(defn lin-search
  [graph]
  (loop [last-humn    1
         current-humn 1
         last-comp    (test-humn graph current-humn)
         current-comp (test-humn graph current-humn)]
    (if (= current-comp 0)
      current-humn
      (if (not= last-comp current-comp)
        [last-humn current-humn]
        (recur current-humn (* 10 current-humn) current-comp (test-humn graph (* 10 current-humn)))))))

(defn bin-search
  [graph min max magic-factor]
  (loop [min min
         max max]
    (let [humn (.longValue (Math/floor (/ (+ min max) 2)))
          test (test-humn graph humn)]
      (do (println humn) (println test))
      (if (= test 0)
        humn
        (if (= test (* 1 magic-factor))
          (recur humn max)
          (recur min humn))))))

; ... and one where we do binary search
(comment
  (time
    (let [graph graph
          magic-factor 1
          [min max] (lin-search graph)] 
      (bin-search graph min max magic-factor))))


