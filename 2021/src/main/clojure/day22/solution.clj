(ns day22
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]))

(def example-instructions-raw
  ["on x=-20..26,y=-36..17,z=-47..7"
   "on x=-20..33,y=-21..23,z=-26..28"
   "on x=-22..28,y=-29..23,z=-38..16"
   "on x=-46..7,y=-6..46,z=-50..-1"
   "on x=-49..1,y=-3..46,z=-24..28"
   "on x=2..47,y=-22..22,z=-23..27"
   "on x=-27..23,y=-28..26,z=-21..29"
   "on x=-39..5,y=-6..47,z=-3..44"
   "on x=-30..21,y=-8..43,z=-13..34"
   "on x=-22..26,y=-27..20,z=-29..19"
   "off x=-48..-32,y=26..41,z=-47..-37"
   "on x=-12..35,y=6..50,z=-50..-2"
   "off x=-48..-32,y=-32..-16,z=-15..-5"
   "on x=-18..26,y=-33..15,z=-7..46"
   "off x=-40..-22,y=-38..-28,z=23..41"
   "on x=-16..35,y=-41..10,z=-47..6"
   "off x=-32..-23,y=11..30,z=-14..3"
   "on x=-49..-5,y=-3..45,z=-29..18"
   "off x=18..30,y=-20..-8,z=-3..13"
   "on x=-41..9,y=-7..43,z=-33..15"
   "on x=-54112..-39298,y=-85059..-49293,z=-27449..7877"
   "on x=967..23432,y=45373..81175,z=27513..53682"])

(defn parse-line
  [line]
  (let [[_ op x1 x2 y1 y2 z1 z2] (re-matches #"(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)" line)]
    {:op (if (= op "on") true false)
     :x1 (Integer/parseInt x1)
     :x2 (Integer/parseInt x2)
     :y1 (Integer/parseInt y1)
     :y2 (Integer/parseInt y2)
     :z1 (Integer/parseInt z1)
     :z2 (Integer/parseInt z2)}))

(def example-instructions
  (->> example-instructions-raw
       (map parse-line)))

(def input-file (io/resource "day22/input.txt"))

(def real-instructions
  (->> (str/split (slurp input-file) #"\n")
       (map parse-line)))

(defn slice
  [start end coll]
  (take (- end start) (drop start coll)))

(def real-instructions-part1 (slice 0 20 real-instructions))

(defn abs
  [n]
  (max n (- n)))

(->> (for [{:keys [op x1 x2 y1 y2 z1 z2]} (slice 0 20 example-instructions)]
       (apply merge (for [x (range x1 (inc x2))
                          y (range y1 (inc y2))
                          z (range z1 (inc z2))
                          :when (and (<= (abs x1) 50) (<= (abs x2) 50) (<= (abs y1) 50) (<= (abs y2) 50) (<= (abs z1) 50) (<= (abs z2) 50))]
                      {[x y z] op})))
     (apply merge) ;; das hier könnte nen bottleneck sein?
     (filter (fn [[k v]] v))
     count
     time)

;; das hier ist zwar schneller, aber das problem ist die :when clause
;; ich vermute er geht durch alle durch, nur um sie dann zu verwerfen
;; was offensichtlich nicht so gut ist
;; part1
(->> (for [{:keys [op x1 x2 y1 y2 z1 z2]} real-instructions-part1
           x (range x1 (inc x2))
           y (range y1 (inc y2))
           z (range z1 (inc z2))
           :when (and (<= (abs x1) 50) (<= (abs x2) 50) (<= (abs y1) 50) (<= (abs y2) 50) (<= (abs z1) 50) (<= (abs z2) 50))]
       [[x y z] op])
     (into {})
     (filter (fn [[k v]] v))
     count
     time) ;; 653798

;; für part2 werde ich einen intelligenteren Ansatz benötigen

(defn between?
  "Arity 3: Returns true iff a <= b <= c
   Arita 2: Returns true if intervals overlap"
  ([a b c]
   (and (<= a b) (<= b c)))
  ([[a b] [c d]]
   (or (between? a c b) (between? c b d) (between? c a d) (between? a d b))))

(defn overlap?
  [{ax1 :x1 ax2 :x2 ay1 :y1 ay2 :y2 az1 :z1 az2 :z2}
   {bx1 :x1 bx2 :x2 by1 :y1 by2 :y2 bz1 :z1 bz2 :z2}]
  (and (between? [ax1 ax2] [bx1 bx2]) (between? [ay1 ay2] [by1 by2]) (between? [az1 az2] [bz1 bz2])))

(defn overlap
  [cube1 cube2]
  (when (overlap? cube1 cube2)
    (let [{ax1 :x1 ax2 :x2 ay1 :y1 ay2 :y2 az1 :z1 az2 :z2 op1 :op} cube1
          {bx1 :x1 bx2 :x2 by1 :y1 by2 :y2 bz1 :z1 bz2 :z2 op2 :op} cube2
          [x1 x2 x3 x4] (sort [ax1 ax2 bx1 bx2])
          [y1 y2 y3 y4] (sort [ay1 ay2 by1 by2])
          [z1 z2 z3 z4] (sort [az1 az2 bz1 bz2])]
      {:x1 x2, :x2 x3, :y1 y2, :y2 y3, :z1 z2, :z2 z3 :op op2})))

; nice idea, but scales horribly
(defn split
  [{ax1 :x1 ax2 :x2 ay1 :y1 ay2 :y2 az1 :z1 az2 :z2 op1 :op}
   {bx1 :x1 bx2 :x2 by1 :y1 by2 :y2 bz1 :z1 bz2 :z2 op2 :op}]
  (let [[x1 x2 x3 x4] (sort [ax1 ax2 bx1 bx2])
        [y1 y2 y3 y4] (sort [ay1 ay2 by1 by2])
        [z1 z2 z3 z4] (sort [az1 az2 bz1 bz2])]
    [{:x1 x2, :x2 x3, :y1 y2, :y2 y3, :z1 z2, :z2 z3 :op op2} ; overlap
     {:x1 x1, :x2 x2, :y1 y1, :y2 y3, :z1 z1, :z2 z3 :op op1} ; links
     {:x1 x3, :x2 x4, :y1 y2, :y2 y4, :z1 z2, :z2 z4 :op op2} ; rechts
     {:x1 x2, :x2 x3, :y1 y1, :y2 y3, :z1 z1, :z2 z2 :op op1} ; vorne
     {:x1 x2, :x2 x3, :y1 y2, :y2 y4, :z1 z3, :z2 z4 :op op2} ; hinten
     {:x1 x2, :x2 x3, :y1 y1, :y2 y2, :z1 z2, :z2 z3 :op op1} ; unten
     {:x1 x2, :x2 x3, :y1 y3, :y2 y4, :z1 z2, :z2 z3 :op op2} ; oben
     ]))

; the combined volume should remain equal after the split
(let [cube1 {:x1 1, :x2 3, :y1 1, :y2 3, :z1 1, :z2 3}
      cube2 {:x1 2, :x2 4, :y1 2, :y2 4, :z1 2, :z2 4}
      cube1 (nth example-instructions 0)
      cube2 (nth example-instructions 1)]
  (=
   (+ (volume cube1) (volume cube2) (- (volume (first (split cube1 cube2)))))
   (->> (split cube1 cube2)
        (map volume)
        (reduce +))))

;; danke Stephan
(defn substract
  "Returns what is left of cube1 after removing the space taken by cube2. Empty cubes are removed.
   Returns an empty vector if cube2 encompasses cube1.
   Return a vector containing exactly cube1 if the cubes don't overlap."
  [cube1 cube2]
  (cond
    (encompasses? cube2 cube1)   []
    (not (overlap? cube1 cube2)) [cube1]
    :else (let [{ax1 :x1 ax2 :x2 ay1 :y1 ay2 :y2 az1 :z1 az2 :z2 op :op} cube1
                {bx1 :x1 bx2 :x2 by1 :y1 by2 :y2 bz1 :z1 bz2 :z2}        cube2
                {ox1 :x1 ox2 :x2 oy1 :y1 oy2 :y2 oz1 :z1 oz2 :z2}        (overlap cube1 cube2)]
            ;; same continuous vs discrete problem, I'm probably double-allotting cubes
            (->> [{:x1 (min ax1 ox1), :x2 (dec ox1), :y1 ay1, :y2 ay2, :z1 az1, :z2 az2 :op op} ; links
                  {:x1 (inc ox2), :x2 (max ax2 ox2), :y1 ay1, :y2 ay2, :z1 az1, :z2 az2 :op op} ; rechts
                  {:x1 ox1, :x2 ox2, :y1 ay1, :y2 ay2, :z1 (min az1 oz1), :z2 (dec oz1) :op op} ; vorne
                  {:x1 ox1, :x2 ox2, :y1 ay1, :y2 ay2, :z1 (inc oz2), :z2 (max az2 oz2) :op op} ; hinten
                  {:x1 ox1, :x2 ox2, :y1 (min ay1 oy1), :y2 (dec oy1), :z1 oz1, :z2 oz2 :op op} ; unten
                  {:x1 ox1, :x2 ox2, :y1 (inc oy2), :y2 (max ay2 oy2), :z1 oz1, :z2 oz2 :op op} ; oben
                  ]
                 (filter #(> (volume %) 0))))))

;; volume is continuous, but cube count should be discrete...
(defn volume
  [{x1 :x1 x2 :x2 y1 :y1 y2 :y2 z1 :z1 z2 :z2}]
  (* (- x2 (dec x1)) (- y2 (dec y1)) (- z2 (dec z1))))

(defn total-volume
  [cubes]
  (->> cubes
       (map volume)
       (reduce +)))

(defn cube-reducer
  [all-cubes new-cube]
  (let [subbed-cubes (mapcat #(substract % new-cube) all-cubes)]
    (if (new-cube :op)
      (conj subbed-cubes new-cube)
      subbed-cubes)))

(->> real-instructions
     (reduce cube-reducer [])
     (map volume)
     (reduce +)
     time) ;; 1257350313518866
