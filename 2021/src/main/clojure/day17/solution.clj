(ns day17
  (:require [clojure.pprint :refer [pprint]]))

;; solution has left-to-down-right assumption
;; puzzle input interval is inclusive on both ends 
(def example-input {:xmin 20
                    :xmax 30
                    :ymin -5
                    :ymax -10})

(def real-input {:xmin 185
                  :xmax 221
                  :ymin -74
                  :ymax -122})

(defn is-in-target-area?
  [{[x y] :pos} {:keys [xmin xmax ymin ymax]}]
  (and (>= x xmin) (<= x xmax) (>= y ymax) (<= y ymin)))

(defn is-past-target-area?
  [{[x y] :pos [x-velo y-velo] :velo} {:keys [xmax ymax]}]
  (or
   (and (> x xmax) (>= x-velo 0))     ;; wenn ich hinter der Area bin und weiter fliege
   (and (< y ymax) (<= y-velo 0))))


(defn int-sums
  ([]  (int-sums 2)) ;; (range 1) => (0)
  ([n] (lazy-seq (cons (reduce + (range n)) (int-sums (inc n))))))

(defn step
  [{:keys [pos velo]}]
  (let [[curr-x curr-y] pos
        [x-velo y-velo] velo
        next-x          (+ curr-x x-velo)
        next-y          (+ curr-y y-velo)
        next-x-velo     (if (zero? x-velo) 0 (+ x-velo (if (neg? x-velo) 1 -1)))
        next-y-velo     (dec y-velo)]
    {:pos [next-x next-y] :velo [next-x-velo next-y-velo]}))

(->> {:pos [0 0] :velo [5 -5]}
     (iterate (partial step))
     (take 3)
     pprint)

(is-past-target-area? {:pos [5 -5], :velo [4 -6]} example-input)

(defn max-height
  [trajectory]
  (apply max (map (fn [{[_ y] :pos}] y) trajectory)))

(take 19 (int-sums))

(defn hits?
  [area trajectory]
  (some #(is-in-target-area? % area) trajectory))

(defn expand-trajectory
  [area start-point]
  (->> start-point
       (iterate (partial step))
       (take-while #(or (is-in-target-area? % area) (not (is-past-target-area? % area))))))

(defn inc-y-velo
  [{ pos :pos [x y] :velo}]
  { :pos pos :velo [x (inc y)]})

(defn maximize-y-velo
  [area trajectory-start]
  (loop [trajectory-start trajectory-start]
    (let [new-trajectory-start (inc-y-velo trajectory-start)]
      (if (hits? area (expand-trajectory area new-trajectory-start))
        (recur new-trajectory-start)
        trajectory-start))))

;; misses a check whether we're hitting at all
(->> {:pos [0 0] :velo [6 10]}
     (iterate (partial step))
     (take-while #(or (is-in-target-area? % example-input) (not (is-past-target-area? % example-input))))
     max-height)

(->> {:pos [0 0] :velo [6 10]}
     (iterate (partial step))
     (take-while #(or (is-in-target-area? % example-input) (not (is-past-target-area? % example-input))))
     (hits? example-input))

;; part1
(let [area              real-input
      min-x-velo        (inc (count (take-while #(< % (area :xmin)) (int-sums))))
      max-x-velo        (area :xmax)
      trajectory-starts (->> (range min-x-velo (inc max-x-velo))
                             (map (fn [x] {:pos [0 0] :velo [x 0]})))]
  (->> trajectory-starts
       (map #(maximize-y-velo area %))
       (map #(expand-trajectory area %))
      ;;  pprint
       (map max-height)
      ;;  (apply max)
       ))

;; okay langsam verstehe ich mein problem
;; selbst wenn ich mit e.g. y=59 nicht lande, heißt das nicht, dass es keine höhere geschwindigkeit gibt, die doch landet
;; das problem lässt sich für teil 1 auch echt massiv herunterbrechen

;; wenn der aufstieg/abstief symmetrisch ist, aka ich immer genau x=0 schneide
;; dann muss die maximale geschwindigkeit die sein, dass ich von 0 auf -122 gehe
;; part 1... oh man
(last (take 121 (int-sums)))
