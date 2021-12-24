(ns day18-2
  (:require [clojure.zip :as z]
            [clojure.pprint :refer [pprint]]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (let [lines (str/split (slurp (io/resource "day18/input.txt")) #"\n")]
             (map #(eval (read-string %)) lines)))

(defn add
  [a b]
  [a b])

(defn find-exploder
  [loc]
  (loop [loc loc]
    (if (and (>= (count (z/path loc)) 4) (vector? (z/node loc)))
      (let [[left right] (z/node loc)]
        (if (and (number? left) (number? right))
          loc))
      (if (z/end? loc)
        nil
        (do
          ;; (println loc)
          (recur (z/next loc)))))))

(defn update-left-old
  [loc val]
  (if-let [left (z/left loc)]
    (z/right (z/edit left #(+ % val)))
    loc))

(defn next-left-leaf
  [loc]
  (if-let [left (z/left loc)]
    (loop [loc      left
           rev-dirs (list z/right)] ;; vector vs list?
      (if (number? (z/node loc))
        [loc rev-dirs]
        (recur (z/prev loc) (conj rev-dirs z/next))))
    [nil []]))

(defn next-leaf
  [loc]
  (loop [loc (-> loc z/next)
         rev [z/prev ]]
    (if (z/end? loc)
      nil
      (if (number? (z/node loc))
        [loc rev]
        (recur (z/next loc) (conj rev z/prev))))))

(defn prev-leaf
  [loc]
  (loop [loc (z/prev loc)
         rev [z/next]]
    (if (nil? loc)
      nil
      (if (number? (z/node loc))
        [loc rev]
        (recur (z/prev loc) (conj rev z/next))))))

(defn update-left
  [loc val]
  (let [[target-loc rev-dirs] (prev-leaf loc)]
    (if (nil? target-loc)
      loc
      (reduce (fn [loc f] (f loc)) (z/edit target-loc #(+ % val)) rev-dirs))))

(defn update-right-old
  [loc val]
  (if-let [right (z/right loc)] ;; right ist nicht ausreichend hier
    (do
      ;; (println (z/node right))
      (z/left (z/edit right #(+ % val))))
    loc))

(defn next-right-leaf
  [loc]
  (if-let [right (z/right loc)]
    (loop [loc right
           rev-dirs (list z/left)]
      (if (number? (z/node loc))
        [loc rev-dirs]
        (recur (z/next loc) (conj rev-dirs z/prev))))
    [nil []]))

(defn update-right
  [loc val]
  (let [[target-loc rev-dirs] (next-leaf loc)]
    (if (nil? target-loc)
      loc
      (reduce (fn [loc f] (f loc)) (z/edit target-loc #(+ % val)) rev-dirs))))

(defn explode
  [loc]
  (let [[left right] (z/node loc)
        new-loc (z/replace loc 0)]
    (-> new-loc
        (update-left left)
        (update-right right))))

(defn find-splitter
  [loc]
  (loop [loc loc]
    (if (and (number? (z/node loc)) (>= (z/node loc) 10))
      loc
      (if (z/end? loc)
        nil
        (recur (z/next loc))))))

(defn split
  [loc]
  (let [new-left    (int (Math/floor (/ (z/node loc) 2)))
        new-right   (int (Math/ceil (/ (z/node loc) 2)))]
    (z/replace loc [new-left new-right])))

(defn reduce-sf-number
  "takes a tree and returns a tree"
  [tree]
  (loop [tree tree]
    (let [loc (z/vector-zip tree)
          next-exploder (find-exploder loc)
          next-splitter (find-splitter loc)]
      (if (some? next-exploder)
        (recur (z/root (explode next-exploder)))
        (if (some? next-splitter)
          (recur (z/root (split next-splitter)))
          (z/root loc))))))
(defn step
  [sfn1 sfn2]
  (reduce-sf-number (add sfn1 sfn2)))

;; let's go!
(reduce step [[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
              [7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
              [[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
              [[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
              [7,[5,[[3,8],[1,4]]]]
              [[2,[2,2]],[8,[8,1]]]
              [2,9]
              [1,[[[9,3],9],[[9,0],[0,7]]]]
              [[[5,[7,4]],7],1]
              [[[[4,2],2],6],[8,7]]])

(defn magnitude
  [node]
  (if (number? node)
    node
    (let [[left right] node]
      (+ (* 3 (magnitude left)) (* 2 (magnitude right))))))

(magnitude [[1,2],[[3,4],5]])
(magnitude [[[[0,7],4],[[7,8],[6,0]]],[8,1]])
(magnitude [[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]])

(def example-input [[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
                    [[[5,[2,8]],4],[5,[[9,9],0]]]
                    [6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
                    [[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
                    [[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
                    [[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
                    [[[[5,4],[7,7]],8],[[8,3],8]]
                    [[9,3],[[9,9],[6,[4,9]]]]
                    [[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
                    [[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]])

;; part1
(magnitude (reduce step input))

;; part2
(apply max (for [sfn1 input
                 sfn2 input]
             (magnitude (step sfn1 sfn2))))
