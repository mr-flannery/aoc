(ns day22.solution
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.set :refer [intersection union]])
  (:import (java.util HashSet)))

(def input (slurp (io/resource "day22/input.txt")))


(def sample-input "        ...#\n        .#..\n        #...\n        ....\n...#.......#\n........#...\n..#....#....\n..........#.\n        ...#....\n        .....#..\n        .#......\n        ......#.\n\n10R5L5R10L4R5L5")

(defn leftpad
  [s n]
  (if (> (count s) n)
    s
    (str s (apply str (repeat (- n (count s)) " ")))))

(def char->kw {\space :void
               \.     :free
               \#     :wall})

(defn parse
  [input]
  (let [[field instructions] (str/split input #"\n\n")
        field-lines         (str/split-lines field)

        parsed-instructions (->> instructions (re-seq #"\d+|[RL]") (map #(cond
                                                                           (= % "R") :R
                                                                           (= % "L") :L
                                                                           :else (Integer/parseInt %))))
        max-l               (apply max (map count field-lines))
        field               (map #(leftpad % max-l) field-lines)
        parsed-field        (into {} (for [l (range 0 (count field))
                                           c (range 0 (count (first field)))]
                                       [[l c] (-> field (nth l) (nth c) char->kw)]))]
    [parsed-field parsed-instructions]))

(def sample-field-and-insts (parse sample-input))
(def field-and-insts (parse input))

(defn next-pos
  [[l c] direction max-l max-c]
  (condp = direction
    :north [(mod (dec l) max-l) c]
    :east [l (mod (inc c) max-c)]
    :south [(mod (inc l) max-l) c]
    :west [l (mod (dec c) max-c)]))

(defn move
  [field from steps direction max-l max-c]
  (loop [remaining-steps steps
         last-safe-pos   from
         last-pos        from]
    (if (= remaining-steps 0)
      last-safe-pos
      (let [next (next-pos last-pos direction max-l max-c)]
        (condp = (get field next)
          :free (recur (dec remaining-steps) next next)
          :void (recur remaining-steps last-safe-pos next)
          :wall (recur 0 last-safe-pos last-pos))))))

(def turn {:north {:R :east :L :west}
           :east  {:R :south :L :north}
           :south {:R :west :L :east}
           :west  {:R :north :L :south}})

(def direction->score {:east  0
                       :south 1
                       :west  2
                       :north 3})

(defn make-moves
  [field instructions initial-pos initial-direction max-l max-c]
  (loop [[inst & insts] instructions
         pos       initial-pos
         direction initial-direction]
    (println inst pos direction)
    (if (nil? inst)
      [[(inc (first pos)) (inc (second pos))] direction]
      (if (number? inst)
        (recur insts (move field pos inst direction max-l max-c) direction)
        (recur insts pos (get-in turn [direction inst]))))))

;part1
(comment
  (time
    (let [[field instructions] field-and-insts
          max-l             (->> field keys (map first) (apply max) inc)
          max-c             (->> field keys (map second) (apply max) inc)
          initial-pos       (->> field (filter (fn [[[l c] v]] (and (= v :free) (= l 0)))) sort first first)
          initial-direction :east
          [[pos-l pos-c] direction] (make-moves field instructions initial-pos initial-direction max-l max-c)]
      (+ (* 1000 pos-l) (* 4 pos-c) (direction->score direction))
      )))

; ===

(def cube-turns {:A {:north [:B :north] :east [:C :north] :south [:F :south] :west [:E :south]}
                 :B {:north [:D :east] :east [:C :east] :south [:A :south] :west [:E :east]}
                 :C {:north [:C :north] :east [:F :west] :south [:A :west] :west [:B :west]}
                 :D {:north [:D :north] :east [:F :north] :south [:C :south] :west [:B :south]}
                 :E {:north [:A :east] :east [:F :east] :south [:D :south] :west [:B :east]}
                 :F {:north [:A :north] :east [:C :west] :south [:D :west] :west [:E :west]}})

(def cube-turn-fns {[:south :south] (fn [[l c]] [0 c])
                    [:east :east]   (fn [[l c]] [l 0])
                    [:east :west]   (fn [[l c]] [(- 49 l) 49])
                    [:west :south]  (fn [[l c]] [0 l])
                    [:west :west]   (fn [[l c]] [l 49])
                    [:north :east]  (fn [[l c]] [c 0])
                    [:west :east]   (fn [[l c]] [(- 49 l) 0])
                    [:east :north]  (fn [[l c]] [49 l])
                    [:north :north] (fn [[l c]] [49 c])
                    [:south :west]  (fn [[l c]] [c 49])})

(def side->offset {:A [150 0]
                   :B [100 0]
                   :C [100 50]
                   :D [50 50]
                   :E [0 50]
                   :F [0 100]})

(defn v+
  [[l1 c1] [l2 c2]]
  [(+ l1 l2) (+ c1 c2)])

(defn v-
  [[l1 c1] [l2 c2]]
  [(- l1 l2) (- c1 c2)])

(defn pos->side
  [[l c]]
  (->> side->offset
       (filter (fn [[side [o-l o-c]]]
                 (let [[n-l n-c] (v- [l c] [o-l o-c])]
                   (and (<= 0 n-l 49) (<= 0 n-c 49)))))
       first
       first))

; I will probably need to write unit tests for this
(defn step2
  [[l c] direction field max-l max-c]
  ;(println "called with " [l c] direction)
  (let [side    (pos->side [l c])
        [n-l n-c] (next-pos [l c] direction max-l max-c)
        field-v (get field [n-l n-c])]
    ;(println "next-pos: " [n-l n-c] field-v)
    (condp = field-v
      :void (let [
                  ;bla (println [[l c] direction])
                  [next-side next-dir] (get-in cube-turns [side direction])
                  ;bla (println [next-side next-dir])
                  [new-l new-c] ((get cube-turn-fns [direction next-dir]) [(mod l 50) (mod c 50)])
                  ;bla (println [new-l new-c])
                  ;TODO: naming
                  next-field-v (get field (v+ (side->offset next-side) [new-l new-c]))
                  ;[[new-new-l new-new-c] new-dir] (step2 (v+ (side->offset next-side) [new-l new-c]) next-dir field max-l max-c)
                  ]
              ;(println "offset: " (side->offset next-side))
              ;(println "new: " [new-l new-c])
              ;(println "result: " (v+ (side->offset next-side) [new-l new-c]))
              ;(println "next-field-v: " next-field-v)
              (if (= :wall next-field-v)
                [[l c] direction]
                [(v+ (side->offset next-side) [new-l new-c]) next-dir])
              ;(println [[new-new-l new-new-c] new-dir] [[new-l new-c] next-dir])
              ;(if (= [[new-new-l new-new-c] new-dir] [[new-l new-c] next-dir])
              ;  [[l c] direction]
              ;  [[new-new-l new-new-c] new-dir])
              )
      :wall [[l c] direction]
      :free [[n-l n-c] direction])))

(defn move2
  [[l c] direction steps field max-l max-c]
  (loop [steps     steps
         [l c] [l c]
         direction direction]
    (if (= 0 steps)
      [[l c] direction]
      (let [[[l c] direction] (step2 [l c] direction field max-l max-c)]
        (recur (dec steps) [l c] direction)))))

(defn follow-instruction2
  [[l c] direction])

(comment
  (time
    (let [[field instructions] field-and-insts
          max-l             (->> field keys (map first) (apply max) inc)
          max-c             (->> field keys (map second) (apply max) inc)
          initial-pos       (->> field (filter (fn [[[l c] v]] (and (= v :free) (= l 0)))) sort first first)
          initial-direction :east]
      (loop [[l c] initial-pos
             direction initial-direction
             [inst & insts] instructions]
        (if (nil? inst)
          (+ (* 1000 (inc l)) (* 4 (inc c)) (direction->score direction))
          (if (number? inst)
            (let [[[l c] direction] (move2 [l c] direction inst field max-l max-c)]
              (recur [l c] direction insts))
            (recur [l c] (get-in turn [direction inst]) insts))))
      )))

(step2 [75 70] :west (first field-and-insts) 200 150)

(nth (second field-and-insts) 1)

