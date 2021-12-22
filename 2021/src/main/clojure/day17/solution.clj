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

(defn intersects-x?
  [{:keys [xmin xmax]} {[x _] :pos}]
  (and (>= x xmin) (<= x xmax)))

(defn hits-x?
  [area trajectory]
  (some #(intersects-x? area %) trajectory))

(defn step-x
  [{[x y] :pos [x-velo y-velo] :velo}]
  (let [next-x          (+ x x-velo)
        next-x-velo     (if (zero? x-velo) 0 (+ x-velo (if (neg? x-velo) 1 -1)))]
    {:pos [next-x y] :velo [next-x-velo y-velo]}))

(defn expand-x-trajectory
  [area start-point]
  (let [n-1 (->> start-point
                 (iterate (partial step-x))
                 (take-while #(and
                               (> (first (% :velo)) 0)
                               (or (intersects-x? area %) (not (is-past-target-area? % area))))))]
    ;; hacky, but whatever
    (conj (into [] n-1) (step-x (last n-1))))) 

(pprint (expand-x-trajectory example-input {:pos [0 0] :velo [6 0]}))

(defn inc-y-velo
  [{pos :pos [x y] :velo}]
  {:pos pos :velo [x (inc y)]})

(defn maximize-y-velo
  [area trajectory-start]
  (loop [trajectory-start trajectory-start]
    (let [new-trajectory-start (inc-y-velo trajectory-start)]
      (if (hits? area (expand-trajectory area new-trajectory-start))
        (recur new-trajectory-start)
        trajectory-start))))

;; okay langsam verstehe ich mein problem
;; selbst wenn ich mit e.g. y=59 nicht lande, heißt das nicht, dass es keine höhere geschwindigkeit gibt, die landet
;; das problem lässt sich für teil 1 auch echt massiv herunterbrechen

;; wenn der aufstieg/abstief symmetrisch ist, aka ich immer genau x=0 schneide
;; dann muss die maximale geschwindigkeit die sein, dass ich von 0 auf -122 gehe
;; part 1... oh man
(last (take 121 (int-sums))) ;; 7381

;; für jedes x mit xmin <= x <= xmax muss es mindestens einen validen y wert geben.
;; wenn ich für jeden x wert den kleinsten y wert finde, muss ich danach nur noch hochzählen
;; für xmax ist der kleinste ywert genau ymax
;; für jeden kleineren x wert muss der minimal y wert mindestens so groß sein wie der kleinste y des vorgängers
;; bzw ich müsste auch einfach konsequent ymax als untere grenze nehmen können

;; alter ich in _die gleiche_ Falle getappt wie in part1
(defn search-all-trajectories-inc-y
  [area trajectory-start]
  (loop [trajectory-start trajectory-start
         result           []
         found-y-min      false]
    (if (hits? area (expand-trajectory area trajectory-start))
      (if found-y-min
        (recur (inc-y-velo trajectory-start) (conj result trajectory-start) found-y-min)
        (recur (inc-y-velo trajectory-start) (conj result trajectory-start) true))
      (if found-y-min
        (do
          ;; (pprint result)
          result)
        (recur (inc-y-velo trajectory-start) result found-y-min)))))

(defn search-all-trajectories-with-limit
  [area limit trajectory-start]
  (loop [trajectory-start   trajectory-start
         result             []]
    (let [{[_ y-velo] :velo} trajectory-start]
      (if (> y-velo limit)
        result
        (if (hits? area (expand-trajectory area trajectory-start))
          (recur (inc-y-velo trajectory-start) (conj result trajectory-start))
          (recur (inc-y-velo trajectory-start) result))))))

;; (search-all-trajectories-inc-y example-input {:pos [0 0] :velo [19 -10]})
(hits? example-input (expand-trajectory example-input {:pos [0 0] :velo [16 -7]}))
(hits-x? example-input (expand-trajectory example-input {:pos [0 0] :velo [6 0]}))

;; es gibt x werte, die niemals treffen
;; und zwar alle, die kleiner als xmin sind, aber größer als ceil(xmax/2) ?

;; uuuund die Lösung ist wirklich mal wieder einfacher als gedacht
;; xmin und xmax war richtig
;; der kleinste y wert ist halt :ymax
;; und der größte ist der kicker aus part1
;; part 2
(let [area              real-input
      limit             (* -1 (inc (area :ymax)))
      min-x-velo        (inc (count (take-while #(< % (area :xmin)) (int-sums))))
      max-x-velo        (area :xmax)
      trajectory-starts (->> (range min-x-velo (inc max-x-velo))
                             (map (fn [x] {:pos [0 0] :velo [x (area :ymax)]})))]
  (->> trajectory-starts
       (mapcat #(search-all-trajectories-with-limit area limit %))
       count))


