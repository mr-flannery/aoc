(ns day11)

(def example-input [[5 4 8 3 1 4 3 2 2 3]
                    [2 7 4 5 8 5 4 7 1 1]
                    [5 2 6 4 5 5 6 1 7 3]
                    [6 1 4 1 3 3 6 1 4 6]
                    [6 3 5 7 3 8 5 4 7 8]
                    [4 1 6 7 5 2 4 6 4 5]
                    [2 1 7 6 8 4 1 7 2 1]
                    [6 8 8 2 8 8 1 1 3 4]
                    [4 8 4 6 8 4 8 5 5 4]
                    [5 2 8 3 7 5 1 5 2 6]])

(def real-input [[5 6 6 5 1 1 4 5 5 4]
                 [4 8 8 2 6 6 5 4 2 7]
                 [6 1 8 5 5 8 2 1 1 3]
                 [7 7 6 2 8 5 2 7 4 4]
                 [7 2 5 5 6 2 1 8 4 1]
                 [8 8 4 2 7 5 3 1 2 3]
                 [8 2 2 5 3 7 2 1 7 6]
                 [7 2 1 2 8 6 5 8 2 7]
                 [7 7 5 8 7 5 1 1 5 7]
                 [1 8 2 8 5 4 4 5 6 3]])

(defn neighbor-coords-w-diag
  [m x y]
  (let [x-max (dec (count (first m)))
        y-max (dec (count m))
        x-coords (cond
                   (= x 0) [x (inc x)]
                   (= x x-max) [(dec x) x]
                   :else [(inc x) x (dec x)])
        y-coords (cond
                   (= y 0) [y (inc y)]
                   (= y y-max) [(dec y) y]
                   :else [(inc y) y (dec y)])]
    (remove #{[x y]} (for [x' x-coords
                           y' y-coords]
                       [x' y']))))

(defn points
  [m]
  (for [y (range (count m))
        x (range (count (first m)))]
    [x y]))

(defn get-point
  [m x y]
  (-> m
      (nth y)
      (nth x)))

(defn update-point
  [m f x y]
  (update-in m [y x] f))

(defn set-point
  [m val x y]
  (assoc-in m [y x] val))

(set-point example-input 0 1 0)

(defn inc-all
  [input]
  (for [row input]
    (map inc row)))

(defn flashers
  ([m]
   (flashers m (points m)))
  ([m points]
   (filter #(> (apply get-point m %) 9) points)))
    

(defn coerce-vec
  [m]
  (into [] (map vec) m))

(defn inc-points
  [m points]
  (reduce (fn [m point] (apply update-point m inc point)) m points))

(clojure.pprint/pprint example-input)

(clojure.pprint/pprint (inc-all example-input))

;; First, the energy level of each octopus increases by 1.
;; 
;; Then, any octopus with an energy level greater than 9 flashes. 
;; This increases the energy level of all adjacent octopuses by 1, including octopuses that are diagonally adjacent.
;; If this causes an octopus to have an energy level greater than 9, it also flashes. 
;; This process continues as long as new octopuses keep having their energy level increased beyond 9. 
;; (An octopus can only flash at most once per step.)
;; 
;; Finally, any octopus that flashed during this step has its energy level set to 0, as it used all of its energy to flash.

(def billo-example [[1 1 1 1 1]
                    [1 9 9 9 1]
                    [1 9 1 9 1]
                    [1 9 9 9 1]
                    [1 1 1 1 1]])

;; todo: refactor
(defn flash-all
  [input]
  (if (empty? (flashers input))
    {:grid input :step-flash-count 0}
    (loop [grid (coerce-vec input)
           f (flashers input)
           flashed []]
      ;; (clojure.pprint/pprint grid)
      (let [next-point (first f)
            neighbors (apply neighbor-coords-w-diag grid next-point)
            next-grid (inc-points grid neighbors)
            flashed (conj flashed next-point)
            remaining-flashers (remove (set flashed) (concat (rest f) (flashers next-grid neighbors)))]
        ;; (println next-point)
        ;; (println flashed)
        (if (empty? remaining-flashers)
          {:grid next-grid :step-flash-count (count flashed)}
          (recur next-grid remaining-flashers flashed))))))

(map #(println %) [[1] [2]])

(defn reset-flashers
  [input]
  (reduce (fn [m point] (apply set-point m 0 point)) input (flashers input)))

(clojure.pprint/pprint
 (-> example-input
     inc-all
     flash-all
     reset-flashers
     ))

(defn step
  [{ :keys [grid flash-count]} step]
  (let [{:keys [grid step-flash-count]} (flash-all (inc-all grid))]
    { :grid (reset-flashers grid) :flash-count (+ flash-count step-flash-count)}))

;; part1
(clojure.pprint/pprint
 (let [steps (range 100)
       input real-input]
   (reduce step {:grid input :flash-count 0} steps))) ;; 1617
