(ns day17.solution
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.set :refer [intersection union]])
  (:import (java.util HashSet)))

(def input (str/trim (slurp (io/resource "day17/input.txt"))))

(def sample-input ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")

(defn parse
  [input])

(defn horizontal-line
  [bot-y]
  #{[2 bot-y] [3 bot-y] [4 bot-y] [5 bot-y]})

(defn plus
  [bot-y]
  #{[3 (+ 2 bot-y)]
    [2 (+ 1 bot-y)] [3 (+ 1 bot-y)] [4 (+ 1 bot-y)]
    [3 bot-y]})

(defn reverse-l
  [bot-y]
  #{[4 (+ 2 bot-y)]
    [4 (+ 1 bot-y)]
    [2 bot-y] [3 bot-y] [4 bot-y]})

(defn vertical-line
  [bot-y]
  #{[2 (+ 3 bot-y)] [2 (+ 2 bot-y)] [2 (+ 1 bot-y)] [2 bot-y]})

(defn block
  [bot-y]
  #{[2 (+ 1 bot-y)] [3 (+ 1 bot-y)]
    [2 bot-y] [3 bot-y]})

(def piece-fns [horizontal-line plus reverse-l vertical-line block])

(defn rotate
  [[h & t]]
  ; concat is lazy, calling rotate too often in the real input causes StackOverflowError
  ; https://stuartsierra.com/2015/04/26/clojure-donts-concat
  ; the better solution is apparently to use into
  ;(doall
  ;  (concat t [h]))
  (into t [h]))

; this also scales horribly
; this effectively makes the whole solution O(n^2)
; idea: take the 10 highest elements, this should safely be enough
(defn pile->set
  [pile]
  (->> pile (reduce union)))

(defn jet-left
  [piece]
  (->> piece
       (map (fn [[x y]] [(dec x) y]))
       (into #{})))

(defn jet-right
  [piece]
  (->> piece
       (map (fn [[x y]] [(inc x) y]))
       (into #{})))

;(defn collides-with-pile?
;  [^HashSet pile piece]
;  (reduce #(or %1 %2) (map #(.contains pile %) piece)))

(defn collides-with-pile?
  [pile piece]
  (not= 0 (count (intersection (pile->set pile) piece))))

(defn apply-jet
  [jet pile piece]
  (if (= jet \<)
    (if (and
          (not= (->> piece (map first) (apply min)) 0)
          (not (collides-with-pile? pile (jet-left piece))))
      (jet-left piece)
      piece)
    (if (and
          (not= (->> piece (map first) (apply max)) 6)
          (not (collides-with-pile? pile (jet-right piece))))
      (jet-right piece)
      piece)))

(defn fall
  [piece]
  (->> piece
       (map (fn [[x y]] [x (dec y)]))
       (into #{})))

; this is probably inefficient
(defn can-fall?
  [pile piece]
  (and
    (not (collides-with-pile? pile (fall piece)))
    (not= 0 (->> piece (map second) (apply min)))))

; this probably also causes scaling issues
; idea: also try take 10 here
; the max should be in the first ten
;(defn top-of-pile
;  [pile]
;  (if (not= 0 (count pile))
;    (->> pile (reduce union) (map second) (apply max))
;    -1))

(defn next-piece
  [piece-fn max-y]
  (piece-fn (+ 4 max-y)))

; rotate input is way too expensive anyway
(def sample-inputv (into [] sample-input))
(def inputv (into [] input))

(defn update-pile
  [pile piece]
  (conj pile piece))

;part1
(comment
  (time
    (->> (let [jets  sample-inputv
               pile  (atom [])
               max-y (atom -1)]
           (loop [piece-fn-counter 0
                  jet-counter      0
                  piece-counter    0
                  piece            (next-piece (get piece-fns piece-fn-counter) @max-y)
                  ]
             ;(if (= 0 (mod piece-counter 10000))
             ;  (println piece-counter))
             (if (= piece-counter
                    ;1000000000000
                    ;100000
                    ;2022
                    ;10
                    )
               (inc @max-y) ; the task seems to start counting at 1, not at 0
               (let [jetted-piece (apply-jet (get jets jet-counter) @pile piece)]
                 ;(println (sort piece))
                 ;(println (get jets jet-counter))
                 ;(println (sort jetted-piece))
                 (if (can-fall? @pile jetted-piece)
                   (recur piece-fn-counter (mod (inc jet-counter) (count jets)) piece-counter (fall jetted-piece))
                   (do
                     (swap! pile update-pile jetted-piece)
                     (let [max-piece-y (->> jetted-piece (map second) (apply max))]
                       ;(println max-piece-y " > " @max-y "?")
                       (if (> max-piece-y @max-y)
                         (do
                           ;(println "swap!") 
                           (swap! max-y (fn [_] max-piece-y)))))
                     (let [new-piece-fn-counter (mod (inc piece-fn-counter) 5)
                           new-piece            (next-piece (get piece-fns new-piece-fn-counter) @max-y)]
                       (recur new-piece-fn-counter (mod (inc jet-counter) (count jets)) (inc piece-counter) new-piece))
                     ))))))
         ;(reduce union)
         ;(map second)
         ;(apply max)
         ;inc ; the task seems to start counting at 1, not at 0
         )))

(defn zip
  [& cols]
  (partition (count cols) (count cols) (apply interleave cols)))

(defn are-halves-equivalent?
  [a-pile a-check-at]
  (let [[first-half second-half] (split-at (/ (count @a-pile) 2) @a-pile)
        first-half-max-y  (->> first-half
                               last
                               (map second)
                               (apply max))
        offset-first-half (->> first-half
                               (map (fn [piece]
                                      (->> piece
                                           (map (fn [[x y]]
                                                  [x (+ y first-half-max-y 1)]))
                                           (into #{})))))
        zipped            (zip offset-first-half second-half)]
    (swap! a-check-at + 10)
    (->> zipped
         (map (fn [[p1 p2]] (= p1 p2)))
         (reduce #(and %1 %2)))
    ))

(count inputv)

(defn are-halves-equivalent?2
  [a-pile a-check-at]
  (swap! a-check-at + 10)
  (let [offset (->> (get @a-pile (/ (count @a-pile) 2)) (map second) (apply max))]
    ;(println offset)
    (loop [i 0]
      (if (= i (/ (count @a-pile) 2))
        true
        (let [offset-a (->> (get @a-pile i) (map (fn [[x y]] [x (+ y offset)])) (into #{}))]
          ;(println offset-a (get @a-pile (+ i (/ (count @a-pile) 2))))
          (if (= offset-a (get @a-pile (+ i (/ (count @a-pile) 2))))
            (recur (inc i))
            false))))))

;part2
; idea: collect the amount of pile growth for all five blocks
(defn growth
  [five-blocks]
  (let [flattened (apply union five-blocks)]
    (- (->> flattened (map second) (apply max)) (->> flattened (map second) (apply min)))))

(defn find-repeating-sequence
  [col]
  (let [growths (reverse col)]
    (loop [i 1]
      (if (= i (/ (count growths) 2))
        nil
        (if (= (take i growths) (->> growths (drop i) (take i)))
          (take i growths)
          (recur (inc i)))))))

(comment
  (time
    (let [pile                            (let [jets     inputv
                                                pile     (atom [])
                                                max-y    (atom -1)
                                                check-at (atom 10)]
                                            (loop [piece-fn-counter 0
                                                   jet-counter      0
                                                   piece-counter    0
                                                   piece            (next-piece (get piece-fns piece-fn-counter) @max-y)]
                                              ;(if (= 0 (mod piece-counter 10000))
                                              ;  (println piece-counter))
                                              (if (= piece-counter
                                                     ;1000000000000
                                                     5000
                                                     ;2022
                                                     ;100
                                                     )
                                                ;(inc @max-y) ; the task seems to start counting at 1, not at 0
                                                @pile
                                                (let [jetted-piece (apply-jet (get jets jet-counter) @pile piece)]
                                                  ;(println (sort piece))
                                                  ;(println (get jets jet-counter))
                                                  ;(println (sort jetted-piece))
                                                  (if (can-fall? @pile jetted-piece)
                                                    (recur piece-fn-counter (mod (inc jet-counter) (count jets)) piece-counter (fall jetted-piece))
                                                    (do
                                                      (swap! pile update-pile jetted-piece)
                                                      (let [max-piece-y (->> jetted-piece (map second) (apply max))]
                                                        ;(println max-piece-y " > " @max-y "?")
                                                        (if (> max-piece-y @max-y)
                                                          (do
                                                            ;(println "swap!") 
                                                            (swap! max-y (fn [_] max-piece-y)))))
                                                      (let [new-piece-fn-counter (mod (inc piece-fn-counter) 5)
                                                            new-piece            (next-piece (get piece-fns new-piece-fn-counter) @max-y)]
                                                        (recur new-piece-fn-counter (mod (inc jet-counter) (count jets)) (inc piece-counter) new-piece))
                                                      ))))))
          bla (println "piles")
          growths                         (->> pile
                                               (partition 5)
                                               (map growth))
          rep-seq                         (find-repeating-sequence growths)
          bla (println rep-seq)
          bottom-end-count                (->> growths
                                               reverse
                                               (partition-all (count rep-seq))
                                               last
                                               count
                                               (* 5))
          bottom-end-max-y                (->> pile
                                               (take bottom-end-count)
                                               (apply union)
                                               (map second)
                                               (apply max))
          bottom-end-plus-one-cycle-max-y (->> pile
                                               (take (+ bottom-end-count (* 5 (count rep-seq))))
                                               (apply union)
                                               (map second)
                                               (apply max))
          growth-per-cycle                (- bottom-end-plus-one-cycle-max-y bottom-end-max-y)
          num-complete-cycles             (quot (- 1000000000000 bottom-end-count) (* 5 (count rep-seq)))
          remaining-top-end               (rem (- 1000000000000 bottom-end-count) (* 5 (count rep-seq)))
          remaining-top-end-grwoth        (- (->> pile
                                                  (take (+ remaining-top-end bottom-end-count))
                                                  (apply union)
                                                  (map second)
                                                  (apply max)

                                                  )
                                             bottom-end-max-y)
          ]
      (+
        bottom-end-max-y
        (*
          growth-per-cycle
          num-complete-cycles)
        remaining-top-end-grwoth
        1 ; because I'm counting from zero
        )
      )))
