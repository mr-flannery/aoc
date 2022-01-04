(ns day24
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.test :refer [is]]
            [clojure.pprint :refer [pprint]]
            [clojure.data.priority-map :as d]))

(def input-file (io/resource "day24/input.txt"))

(def real-instructions (str/split (slurp input-file) #"\n"))

(def sub-programs (->> real-instructions
                       (partition-by #(str/starts-with? % "inp"))
                       (partition 2)
                       (map (fn [[a b]] (concat a b)))))

(def input-file2 (io/resource "day24/input-commented.txt"))

(def real-instructions2 (str/split (slurp input-file2) #"\n"))

(def sub-programs2 (->> real-instructions2
                       (partition-by #(str/starts-with? % "inp"))
                       (partition 2)
                       (map (fn [[a b]] (concat a b)))))

(defn slice
  [start end coll]
  (take (- end start) (drop start coll)))

(def w (atom 0))
(def x (atom 0))
(def y (atom 0))
(def z (atom 0))

(defn current-state
  []
  {:w @w :x @x :y @y :z @z})

(defn reset-state
  []
  (do
    (reset! w 0)
    (reset! x 0)
    (reset! y 0)
    (reset! z 0)))

;; not even sure if a macro helps me that much here...
;; update: turns out it does not
(defmacro monad
  [[f at1 at2]]
  `(reset! ~at1 (~f @~at1 @~at2)))

(defn inp
  [at n]
  (reset! at n))

(defn monad-op
  [f atom-a atom-b]
  (let [val-b (if (instance? clojure.lang.Atom atom-b) @atom-b atom-b)]
    (reset! atom-a (f @atom-a val-b))))

(defn add
  [atom-a atom-b]
  (monad-op + atom-a atom-b))

(defn mul
  [atom-a atom-b]
  (monad-op * atom-a atom-b))

(defn div
  [atom-a atom-b]
  (monad-op (comp int /) atom-a atom-b))

;; this is a little iffy..
(defn mod
  [atom-a atom-b]
  (monad-op clojure.core/mod atom-a atom-b))

(defn equal
  [a b]
  (if (= a b) 1 0))

(defn eql
  [atom-a atom-b]
  (monad-op equal atom-a atom-b))

(defn exec-inp
  [inp-s val]
  (eval (read-string (str "(" inp-s " " val ")"))))

(defn exec-op
  [op-s]
  (eval (read-string (str "(" op-s ")"))))

(defn run-monad
  [args instructions]
  {:pre [(is (= (count args) (count (filter #(str/starts-with? % "inp") instructions))))]}
  (loop [args args
         instructions instructions]
    (if (empty? instructions)
      nil
      (let [next-instruction (first instructions)]
        (if (.startsWith next-instruction "inp")
          (do
            (exec-inp next-instruction (first args))
            (recur (rest args) (rest instructions)))
          (do
            (exec-op next-instruction)
            (recur args (rest instructions))))))))

;; demo
(run-monad [1 4] ["inp z"
                  "inp x"
                  "mul z 3"
                  "eql z x"])

(comment
  ;; throws an assertion error if the number of arguments does not match the number of inp statements
  (run-monad [1] ["inp z"
                  "inp x"
                  "mul z 3"
                  "eql z x"]))

(defn run-monad-once
  [instructions args]
  (do
    (reset-state)
    (time (run-monad args instructions))
    (current-state)))

;; w seems to be purely used for storing the input
;; (run-monad-once [1 9 7 6] (apply concat (take 4 sub-programs))) => {:w 6, :x 0, :y 0, :z 232}
;; that's the first time :x or :y ever turned to zero
;; not sure if this is good or bad

(map #(run-monad-once (apply concat (take 1 sub-programs)) (vector %)) (range 0 10))

;; probably makes sense to find out what the individual programs do
;; program 1 sets y and z to w + 7
(map #(run-monad-once (apply concat (take 1 sub-programs)) (vector %)) (range 0 10))

;; program 2 sets y to w + 15 and z to w + 223
(map #(run-monad-once (apply concat (take 2 sub-programs)) (conj [1] %)) (range 0 10))

;; program 3 sets y = w + 2 and z to w + 5826 
;; (at least for these previous arguments... the z value changes when changing the previous arguments)
(map #(run-monad-once (apply concat (take 3 sub-programs)) (conj [1 1] %)) (range 0 10))

;; okay here is where stuff gets interesting
;; when arg3 == arg4 + 1 (e.g. [1 1 4 3]) then the resulting state will have x=0, y=0 and z=224
(map #(run-monad-once (apply concat (take 4 sub-programs)) (conj [1 1 4] %)) (range 0 10))

;; this only seems to impact this step in that the resulting z value will be significantly lower (~5.000 vs ~150.000)
(map #(run-monad-once (apply concat (take 5 sub-programs)) (conj [1 1 2 1] %)) (range 0 10))

;; and now for whatever reason w = 6 will always produce the odd one out
(map #(run-monad-once (apply concat (take 6 sub-programs)) (conj [1 1 3 3 1] %)) (range 0 10))

(map #(run-monad-once (apply concat (take 7 sub-programs)) (conj [1 1 3 2 1 6] %)) (range 0 10))

(map #(run-monad-once (apply concat (take 8 sub-programs)) (conj [1 1 3 2 1 6 1] %)) (range 0 10))

(map #(run-monad-once (apply concat (take 9 sub-programs)) (conj [1 1 3 2 1 6 1 9] %)) (range 0 10))

(map #(run-monad-once (apply concat (take 10 sub-programs)) (conj [1 1 3 2 1 6 1 9 5] %)) (range 0 10))

;; in general it seems like some specific values tend to trigger specific behavior
;; and linear effects in one step tend to propagate through the calls
(map #(run-monad-once (apply concat (take 11 sub-programs)) (conj [1 1 3 2 1 6 1 9 5 4] %)) (range 0 10))

(map #(run-monad-once (apply concat (take 12 sub-programs)) (conj [1 1 3 2 1 6 1 9 5 4 1] %)) (range 0 10))

(map #(run-monad-once (apply concat (take 13 sub-programs)) (conj [1 1 3 2 1 6 1 9 5 4 1 1] %)) (range 0 10))

(map #(run-monad-once (apply concat (take 14 sub-programs)) (conj [1 1 3 2 1 6 1 9 5 4 1 1 1] %)) (range 0 10))

;; arg12 = 7 produces something different...
(run-monad-once real-instructions [1 1 9 8 1 6 1 9 5 4 1 7 1 3])

;; some positions seem to require fixed inputs (which would drastically cut the search space)
;; arg4 = arg3 + 1 (so 9 8 would make sense)
;; arg6 = 6
;; arg8 = 9
;; arg9 = 5
;; arg10 = 4
;; arg12 = 7 (potentially)
;; arg14 = 3

;; this one is a winner [1 1 4 3 1 6 1 9 5 4 1 7 1 3]

;; welp...
(def search-space (for [arg1  (range 9 0 -1)
                        arg2  (range 9 0 -1)
                        ;; arg5  (range 9 0 -1)
                        ;; arg7  (range 9 0 -1)
                        ;; arg11 (range 9 0 -1)
                        ;; arg13 (range 9 0 -1)
                        ]
                    [arg1 arg2 9 8 1 6 1 9 5 4 1 7 1 3]))

(->> (lazy-seq search-space)
     (take 100)
     (map (fn [args] [args (run-monad-once real-instructions args)]))
     )

;; let's try from the back
(map #(run-monad-once (apply concat (slice 12 14 sub-programs)) (conj [9] %)) (range 0 10))
;; [3 6] in the back seems to work? [4 7] [5 8] [6 9]
;; ne halt andersrum...

(map #(run-monad-once (apply concat (slice 11 14 sub-programs)) (concat [%] [6 9])) (range 0 10))

(map #(run-monad-once (apply concat (slice 10 14 sub-programs)) (concat [%] [1 6 9])) (range 0 10))

(map #(run-monad-once (apply concat (slice 9 14 sub-programs)) (concat [%] [1 1 6 9])) (range 0 10))

(run-monad-once (slice 0 4 (nth sub-programs 1)) [6])

(=
 (map #(run-monad-once (apply concat (take 1 sub-programs)) (vector %)) (range 0 10))
 (map #(run-monad-once (apply concat (take 1 sub-programs2)) (vector %)) (range 0 10)))

(=
 (map #(run-monad-once (apply concat (take 2 sub-programs)) (conj [1] %)) (range 0 10))
 (map #(run-monad-once (apply concat (take 2 sub-programs2)) (conj [2] %)) (range 0 10)))

(=
 (map #(run-monad-once (apply concat (take 3 sub-programs)) (conj [1 1] %)) (range 0 10))
 (map #(run-monad-once (apply concat (take 3 sub-programs2)) (conj [1 1] %)) (range 0 10)))

(=
 (map #(run-monad-once (apply concat (take 4 sub-programs)) (conj [1 1 1] %)) (range 0 10))
 (map #(run-monad-once (apply concat (take 4 sub-programs2)) (conj [1 1 1] %)) (range 0 10)))

(run-monad-once real-instructions [9 9 9 9 9 9 9 9 9 9 9 9 9 9])

(defn next-args-candidates
  [args]
  (let [idx (filter #(> (get args %) 1) (range 0 14))]
    (map #(update-in args [%] dec) idx)))

(loop [
       queue (into (d/priority-map) [[[9 9 9 9 9 9 9 9 9 9 9 9 9 9] ((run-monad-once real-instructions [9 9 9 9 9 9 9 9 9 9 9 9 9 9]) :z)]])
       n 0]
  (if (= n 10)
    queue
    (let [[args _]     (first queue)
          next-args-cs (next-args-candidates args)
          next-cs      (map (fn [a] [a ((run-monad-once real-instructions a) :z)]) next-args-cs)]
      ;; next-cs
      (recur (apply conj queue next-cs) (inc n))
      )))

(apply conj (into (d/priority-map) [[1 1]]) [[2 3]])