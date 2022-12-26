(ns day20.solution
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.set :refer [intersection union]])
  (:import (java.util HashSet ArrayList)))

(def input (slurp (io/resource "day20/input.txt")))

(def sample-input "1\n2\n-3\n3\n-2\n0\n4\n")

(defn parse
  [input]
  (->> (str/split-lines input)
       (map #(Integer/parseInt %))
       (map (fn [n] {:n n :moved? false}))
       (into [])))

(def sample-file (parse sample-input))
(def real-file (parse input))

; find the index of the first element that has not been moved
; take everything except that element
; insert the other element at it's original index plus the value

(defn find-idx
  [f col]
  (loop [[h & t] col
         idx 0]
    (if (nil? h)
      nil
      (if (f h)
        idx
        (recur t (inc idx))))))

; irgendwas ist hier weird
; wenn ich ein item um (len col) bewege, sollte es eigentlich an seiner usprünglichen position rauskommen
; was ich noch nicht checke ist dieses insert am ende oder am anfang
; im sinne von der letzte index einer col mit length n ist halt n-1
; aber 0 ist quasi die gleiche position
; weil wenn ich eine col auf die andere shiften kann sind die äquivalent
; aber der versaut mir das inserten, weil unter indizes n-1 und 0 halt nicht das gleiche sind
; die frage ist also auch wann ich welchen wo einfüge
; oder ob das überhaupt ne rolle spielt
; vllt sind indizes hier auch einfach das falsche mentale modell
; ooooderr.. es funktioniert halt einfach schon :shrug:
(defn insert-at
  [item n col]
  (if (= 0 (mod n (count col)))
    (concat col [item])
    (concat (take (mod n (count col)) col) [item] (drop (mod n (count col)) col))
    ;(concat (take (+ (quot n (count col)) (mod n (count col))) col) [item] (drop (+ (quot n (count col)) (mod n (count col))) col))
    ;(if (neg? n) ; kann es sein, dass ich für jeden wrap around 1 in die jeweilige Richtung drauflegen muss?
    ;  (concat (take (mod n (count col)) col) [item] (drop (mod n (count col)) col))
    ;  (concat (take (mod n (inc (count col))) col) [item] (drop (mod n (inc (count col))) col)))
    ))

(defn move-next-item
  [col]
  (if-let [idx (find-idx #(not (:moved? %)) col)]
    (let [[processed [item & remaining]] (split-at idx col)]
      (insert-at (assoc item :moved? true) (+ idx (item :n)) (concat processed remaining)))
    nil))

;part1
(comment
  (time
    (let [res (->>
                (loop [col real-file]
                  (let [new-col (move-next-item col)]
                    (if (nil? new-col)
                      col
                      (do
                        ;(println new-col)
                        ;(println (map :n new-col))
                        (recur new-col)))))
                (map :n))
          idx (find-idx zero? res)]
      (+
        (nth res (mod (+ idx 1000) (count res)))
        (nth res (mod (+ idx 2000) (count res)))
        (nth res (mod (+ idx 3000) (count res)))))))

; ======
(def decrpytion-key 811589153)

(defn part2ify
  [file]
  (loop [i    0
         file file]
    (if (= i (count file))
      file
      (recur (inc i) (-> file
                         (update-in [i :n] #(* % decrpytion-key))
                         (assoc-in [i :pos] i))))))

(def sample-file-2 (part2ify sample-file))
(def real-file-2 (part2ify real-file))

(defn move-next-item-2
  [i col]
  (if-let [idx (find-idx #(= i (:pos %)) col)]
    (let [[processed [item & remaining]] (split-at idx col)]
      (insert-at (assoc item :moved? true) (+ idx (item :n)) (concat processed remaining)))
    nil))

(defn mix-2
  [file]
  (loop [i    0
         file file]
    (if (= i (count file))
      file
      (do
        (recur (inc i) (move-next-item-2 i file))))))

;part2
(let [res (->>
            (loop [i    0
                   file real-file-2]
              (if (= i 10)
                file
                (recur (inc i) (mix-2 file))))
            (map :n))
      idx (find-idx zero? res)]
  (+
    (nth res (mod (+ idx 1000) (count res)))
    (nth res (mod (+ idx 2000) (count res)))
    (nth res (mod (+ idx 3000) (count res)))))
