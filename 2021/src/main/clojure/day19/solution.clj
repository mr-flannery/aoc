(ns day19
  (:require [clojure.pprint :refer [pprint]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.zip :as z]
            [clojure.set :refer [difference subset? superset?]]))

(def scans0 [[-618,-824,-621]
             [-537,-823,-458]
             [-447,-329,318]
             [404,-588,-901]
             [544,-627,-890]
             [528,-643,409]
             [-661,-816,-575]
             [390,-675,-793]
             [423,-701,434]
             [-345,-311,381]
             [459,-707,401]
             [-485,-357,347]])

(def scans1 [[686,422,578]
             [605,423,415]
             [515,917,-361]
             [-336,658,858]
             [-476,619,847]
             [-460,603,-452]
             [729,430,532]
             [-322,571,750]
             [-355,545,-477]
             [413,935,-424]
             [-391,539,-444]
             [553,889,-390]])

(apply + (first scans0))
(apply + (first scans1))

(def scanners [[[404,-588,-901]
                [528,-643,409]
                [-838,591,734]
                [390,-675,-793]
                [-537,-823,-458]
                [-485,-357,347]
                [-345,-311,381]
                [-661,-816,-575]
                [-876,649,763]
                [-618,-824,-621]
                [553,345,-567]
                [474,580,667]
                [-447,-329,318]
                [-584,868,-557]
                [544,-627,-890]
                [564,392,-477]
                [455,729,728]
                [-892,524,684]
                [-689,845,-530]
                [423,-701,434]
                [7,-33,-71]
                [630,319,-379]
                [443,580,662]
                [-789,900,-551]
                [459,-707,401]]
               [[686,422,578]
                [605,423,415]
                [515,917,-361]
                [-336,658,858]
                [95,138,22]
                [-476,619,847]
                [-340,-569,-846]
                [567,-361,727]
                [-460,603,-452]
                [669,-402,600]
                [729,430,532]
                [-500,-761,534]
                [-322,571,750]
                [-466,-666,-811]
                [-429,-592,574]
                [-355,545,-477]
                [703,-491,-529]
                [-328,-685,520]
                [413,935,-424]
                [-391,539,-444]
                [586,-435,557]
                [-364,-763,-893]
                [807,-499,-711]
                [755,-354,-619]
                [553,889,-390]]
               [[649,640,665]
                [682,-795,504]
                [-784,533,-524]
                [-644,584,-595]
                [-588,-843,648]
                [-30,6,44]
                [-674,560,763]
                [500,723,-460]
                [609,671,-379]
                [-555,-800,653]
                [-675,-892,-343]
                [697,-426,-610]
                [578,704,681]
                [493,664,-388]
                [-671,-858,530]
                [-667,343,800]
                [571,-461,-707]
                [-138,-166,112]
                [-889,563,-600]
                [646,-828,498]
                [640,759,510]
                [-630,509,768]
                [-681,-892,-333]
                [673,-379,-804]
                [-742,-814,-386]
                [577,-820,562]]
               [[-589,542,597]
                [605,-692,669]
                [-500,565,-823]
                [-660,373,557]
                [-458,-679,-417]
                [-488,449,543]
                [-626,468,-788]
                [338,-750,-386]
                [528,-832,-391]
                [562,-778,733]
                [-938,-730,414]
                [543,643,-506]
                [-524,371,-870]
                [407,773,750]
                [-104,29,83]
                [378,-903,-323]
                [-778,-728,485]
                [426,699,580]
                [-438,-605,-362]
                [-469,-447,-387]
                [509,732,623]
                [647,635,-688]
                [-868,-804,481]
                [614,-800,639]
                [595,780,-596]]
               [[727,592,562]
                [-293,-554,779]
                [441,611,-461]
                [-714,465,-776]
                [-743,427,-804]
                [-660,-479,-426]
                [832,-632,460]
                [927,-485,-438]
                [408,393,-506]
                [466,436,-512]
                [110,16,151]
                [-258,-428,682]
                [-393,719,612]
                [-211,-452,876]
                [808,-476,-593]
                [-575,615,604]
                [-485,667,467]
                [-680,325,-822]
                [-627,-443,-432]
                [872,-547,-609]
                [833,512,582]
                [807,604,487]
                [839,-516,451]
                [891,-625,532]
                [-652,-548,-490]
                [30,-46,-14]]])

(def input-file (io/resource "day19/input.txt"))

(def real-scanners (->> (str/split (slurp input-file) #"--- scanner \d+ ---\n")
                         (remove empty?)
                         (map #(str/split % #"\n"))
                         (map #(map (fn [s] (str "[" s "]")) %))
                         (map #(map (fn [s] (eval (read-string s))) %))
                         (map #(into [] %))))

(def rotation-vectors (for [x [1 -1]
                            y [1 -1]
                            z [1 -1]]
                        [x y z]))


(defn abs
  [n]
  (max n (- n)))

(defn v+
  [a b]
  (->> (map vector a b)
       (map #(apply + %))))

(defn v-neg
  [[x y z]]
  [(- x) (- y) (- z)])

(defn v-offset
  [a b]
  (->> (map vector (map abs a) (map abs b))
       (map #(apply + %))))

(defn v-
  [a b]
  (->> (map vector a b)
       (map #(apply - %))))

(defn v-abs-diff
  [a b]
  (->> (map vector (map abs a) (map abs b))
       (map #(apply - %))))

(defn v*
  [a b]
  (->> (map vector a b)
       (map #(apply * %))))

;; assuming I treat scanner0 as [1 1 1], I now know the offset and rotation of scanner1
;; (let [rot-and-offs (for [[rot1 scan1-rots] (for [rot rotation-vectors]
;;                                              [rot (map #(v* rot %) scans1)])]
;;                      [rot1 (->> (map vector scans0 scan1-rots)
;;                                 (map #(apply v+ %)))])]
;;   (->> rot-and-offs
;;        (filter (fn [[_ offs]] (= 1 (count (set offs)))))
;;        first
;;        pprint))

;; wenn ich einen scaner fix lasse
;; und für den anderen scanner alle rotationen nehme
;; und dann für diese 8 paare alle Punkte paarweise vergleiche
;; bekomme ich am ende einen Überblick, wie viele dieser Vergleiche den gleichen offset produziert haben
(defn rot-and-pairwise-offsets
  [scanner1 scanner2]
  (for [[rot1 scan1-rots] (for [rot rotation-vectors]
                            [rot (map #(v* rot %) scanner2)])]
    [rot1 (for [beacon0 scanner1
                beacon1 scan1-rots]
            {:b1 beacon0 :b2 beacon1 :offset (v+ beacon0 beacon1)})]))

;; (defn overlaps
;;   "returns a vector of pairs of rotation and overlaps"
;;   [scanner1 scanner2]
;;   (for [[rot offsets] (rot-and-pairwise-offsets scanner1 scanner2)
;;         :when (some #(>= (count %) 12) (vals (group-by :offset offsets)))]
;;     [rot offsets]))

(defn mapped-points
  [scanner1 scanner2]
  (let [overlaps (overlaps scanner1 scanner2)]
    (if (empty? overlaps)
      nil
      (let [[rot comps] (first overlaps)]
        [rot (->> comps
                  (group-by :offset)
                  (sort-by #(count (second %)) >)
                  first
                  second
                  (map (fn [{b1 :b1 b2 :b2}] [b1 b2])))]))))

(comment
  (pprint (let [scanner1 (get scanners 0)
                scanner2 (get scanners 1)
                mapped-points (mapped-points scanner1 scanner2)]
            (concat (remove (set (map first mapped-points)) scanner1) mapped-points))))

(defn symmetric-unique-pairs
  [n]
  (mapcat identity (for [a (range n)]
                     (for [b (range (inc a) n)]
                       [a b]))))

(defn asymmetric-unique-pairs 
  [n]
  (mapcat identity (for [a (range n)]
                     (for [b (range n)
                           :when (not= a b)]
                       [a b]))))

;; so und jetzt alles zusammen
;; waaarum krieg ich 1 und 4 nicht zusammen?
(comment 
  (def comps (->> (let [pairs unsymmetric-unique-pairs]
                  (for [[s1 s2] pairs]
                    [[s1 s2]
                     (let [s1                       (get scanners s1)
                           s2                       (get scanners s2)
                           rot-and-pairwise-offsets (for [[rot1 scan1-rots] (for [rot rotation-vectors]
                                                                              [rot (map #(v* rot %) s2)])]
                                                      [rot1 (for [beacon0 s1
                                                                  beacon1 scan1-rots]
                                                              (v+ beacon0 beacon1))])]
                       (for [[rot pairwise-offsets] rot-and-pairwise-offsets]
                         [rot (sort-by val > (frequencies pairwise-offsets))]))]))
                (into {}))))

(comment
  (pprint (for [[s1 s2] symmetric-unique-pairs]
            (let [scanner1 (get scanners s1)
                  scanner2 (get scanners s2)]
              (mapped-points scanner1 scanner2)))))

(comment
  (let [[rot points] (mapped-points (get scanners 1) (get scanners 3))]
    (->> points
         (map first)
         (map #(v* % [1 -1 1]))
         (map #(v+ % [68,-1246,-43]))
         (map #(v* % [-1 -1 -1])))))

(def rot24-fns [(fn [[x y z]] [x y z])
                (fn [[x y z]] [x (- y) (- z)])
                (fn [[x y z]] [x (- z) y])
                (fn [[x y z]] [x z (- y)])
                (fn [[x y z]] [(- x) y (- z)])
                (fn [[x y z]] [(- x) (- y) z])
                (fn [[x y z]] [(- x) (- z) (- y)])
                (fn [[x y z]] [(- x) z y])
                (fn [[x y z]] [y (- x) z])
                (fn [[x y z]] [y x (- z)])
                (fn [[x y z]] [y z x])
                (fn [[x y z]] [y (- z) (- x)])
                (fn [[x y z]] [(- y) x z])
                (fn [[x y z]] [(- y) (- x) (- z)])
                (fn [[x y z]] [(- y) z (- x)])
                (fn [[x y z]] [(- y) (- z) x])
                (fn [[x y z]] [z y (- x)])
                (fn [[x y z]] [z (- y) x])
                (fn [[x y z]] [z x y])
                (fn [[x y z]] [z (- x) (- y)])
                (fn [[x y z]] [(- z) y x])
                (fn [[x y z]] [(- z) (- x) (- y)])
                (fn [[x y z]] [(- z) y (- x)])
                (fn [[x y z]] [(- z) (- y) x])])

(defn overlaps
  [scanner1 scanner2]
  (let [s2-rots                (for [rot rot24-fns]
                                 {:rot-fn rot :beacon-pairs (map vector scanner2 (map rot scanner2))})
        pairwise-comps-per-rot (for [{rot-fn :rot-fn s2 :beacon-pairs} s2-rots]
                                 (->> (for [b1             scanner1
                                            [og-b2 rot-b2] s2]
                                        {:b1 b1 :b2 rot-b2 :og-b2 og-b2 :s-dist (v+ b1 (v-neg rot-b2))})
                                      (group-by :s-dist)
                                      (map (fn [[s-dist beacons]] {:beacons beacons :count (count beacons) :rot-fn rot-fn}))
                                      (sort-by :count >)))]
    (->> pairwise-comps-per-rot
         (map first)
         (filter #(>= (:count %) 12)))))

(comment
  (let [matches (for [[i1 i2] symmetric-unique-pairs
                      :let [overlaps (overlaps (get scanners i1) (get scanners i2))]
                      :when (not (empty? overlaps))]
                  {:s1 i1 :s2 i2 :overlaps (first overlaps)})
        dist    [68 -1246 -43]]
    (let [{{beacons14 :beacons
            rot-fn14  :rot-fn} :overlaps} (-> matches
                                              (nth 2))
          {{beacons01 :beacons
            rot-fn01  :rot-fn} :overlaps} (-> matches
                                              (nth 0))]
      (->> beacons14
           (map :b1)
           (map rot-fn01)
           (map #(v+ dist %))
           pprint))))

(defn matches
  [scanners]
  (for [[i1 i2] (symmetric-unique-pairs (count scanners))
        :let [overlaps (overlaps (nth scanners i1) (nth scanners i2))]
        :when (not (empty? overlaps))]
    {:s1 i1 :s2 i2 :overlaps (first overlaps)}))

(defn matches2
  [scanners]
  (for [[i1 i2] (asymmetric-unique-pairs (count scanners))
        :let [overlaps (overlaps (nth scanners i1) (nth scanners i2))]
        :when (not (empty? overlaps))]
    {:s1 i1 :s2 i2 :overlaps (first overlaps)}))

(defn matches3
  [scanners]
  (for [[i1 i2] (symmetric-unique-pairs (count scanners))
        :let [overlaps (overlaps (nth scanners i1) (nth scanners i2))]
        :when (not (empty? overlaps))]
    (do
      ;; (println (count overlaps))
      {:s1 i1 :s2 i2 :overlaps (first overlaps)})))

(defn count-all-matches
  [scanners]
  (let [matches (matches scanners)]
    (->> matches
         (map :overlaps)
         (map :beacons)
         (map count)
         (reduce +))))

;; für den Beispiel Input sind tatsächlich alle matches unique
(- (reduce + (map count scanners)) (count-all-matches scanners))

;; part1
(def real-matches (matches real-scanners))
(def real-matches2 (matches2 real-scanners))

;; (defn insert-beacon
;;   [all-beacons beacon]
;;   (if (map? beacon)
;;     (if-let [candidate (first (filter #(or (.contains % (beacon :b1)) (.contains % (beacon :b2))) all-beacons))]
;;       (concat (remove candidate all-beacons) (conj candidate (beacon :b1) (beacon :b2)))
;;       (conj all-beacons #{beacon}))
;;     (if-let [candidate (first (filter #(.contains % beacon) all-beacons))]
;;       (concat (remove candidate all-beacons) (conj candidate beacon))
;;       (conj all-beacons #{beacon}))))

(defn insert-edge
  [graph edge]
  (if (contains? graph (edge :s1))
    (update-in graph [(edge :s1)] #(conj % (edge :s2)))
    (assoc graph (edge :s1) [(edge :s2)])))

(def overlap-graph (->> real-matches
                        (reduce insert-edge {})))
overlap-graph

(defn path
  [graph from to]
  (loop [stack [[from [from]]]]
    (if (empty? stack)
      nil
      (let [[next path] (peek stack)]
        (if (= next to)
          path
          (recur (reduce (fn [stack node] (conj stack [node (conj path node)])) (pop stack) (get graph next))))))))

(defn dfs
  [graph start]
  (loop [stack [start]
         visited #{}]
    (if (empty? stack)
      visited
      (let [next (peek stack)]
        (recur (into (pop stack) (get graph next)) (conj visited next))))))

()

(dfs overlap-graph 0)
(map #(dfs overlap-graph %) (range 36))
(def connnected-components
  (reduce (fn [components comp]
            (if-let [subset (first (filter #(subset? % comp) components))]
              (conj (remove (set subset) components) comp)
              (if (empty? (filter #(superset? % comp) components))
                (conj components comp)
                components)))
          #{}
          (map #(dfs overlap-graph %) (range 36))))

connnected-components

;; index matches
(def indexed-matches (->> real-matches
                          (map (fn [m] [[(m :s1) (m :s2)] m]))
                          (into {})))

(map first (group-by first (keys indexed-matches)))

(->> connnected-components
     first
     (partition 2 1)
     (map #(into [] %))
     first
     (get indexed-matches))


;; ich muss echt erstmal alle Punkte nach 0 normalisieren
;; oder vllt auch nicht!
(comment (->> real-matches
     (mapcat #((% :overlaps) :beacons))
     (reduce insert-beacon #{})
     count))

(def real-matches3 (matches3 real-scanners))

;; all points have unique coordinates
;; so it should be fine to look through all matches and keep track of the points I have already accounted for
;; and then do the same for all points
(= (count (set (mapcat identity real-scanners))) (count (mapcat identity real-scanners)))

(let [[unique-beacons known-beacons] (loop [queue  (->> real-matches3
                                                        (mapcat (comp :beacons :overlaps)))
                                            known  #{}
                                            result []]
                                       (if (empty? queue)
                                         [result known]
                                         (let [{b1 :b1 b2 :og-b2} (first queue)]
                                           (if (or (.contains known b1) (.contains known b2))
                                             (recur (rest queue) (conj known b1 b2) result)
                                             (recur (rest queue) (conj known b1 b2) (conj result b1))))))
      unmatched-beacons              (filter #(not (.contains known-beacons %)) (mapcat identity real-scanners))]
  ;; (count unique-beacons)
  ;; (+ (count unique-beacons) (count unmatched-beacons))
  (println "all " (count (mapcat identity real-scanners)))
  (println "known "(count known-beacons))
  (println "unique " (count unique-beacons))
  (println "unmatched " (count unmatched-beacons))
  (println "all - known " (count (remove known-beacons (mapcat identity real-scanners))))
  (println "all - unique " (count (remove (set unmatched-beacons) (mapcat identity real-scanners))))
  )

(let [[unique-beacons known-beacons] (loop [queue  (->> (matches3 scanners)
                                                        (mapcat (comp :beacons :overlaps)))
                                            known  #{}
                                            result []]
                                       (if (empty? queue)
                                         [result known]
                                         (let [{b1 :b1 b2 :og-b2} (first queue)]
                                           (if (or (.contains known b1) (.contains known b2))
                                             (recur (rest queue) (conj known b1 b2) result)
                                             (recur (rest queue) (conj known b1 b2) (conj result b1))))))
      unmatched-beacons              (filter #(not (.contains known-beacons %)) (mapcat identity scanners))]
  (println "all " (count (mapcat identity scanners)))
  (println "known " (count known-beacons))
  (println "unique " (count unique-beacons))
  (println "unmatched " (count unmatched-beacons))
  (println "unique + unmatched " (+ (count unique-beacons) (count unmatched-beacons)))
  )