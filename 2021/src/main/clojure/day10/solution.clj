(ns day10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def example-input ["[({(<(())[]>[[{[]{<()<>>"
                    "[(()[<>])]({[<{<<[]>>("
                    "{([(<{}[<>[]}>{[]{[(<()>"
                    "(((({<>}<{<{<>}{[]{[]{}"
                    "[[<[([]))<([[{}[[()]]]"
                    "[{[{({}]{}}([{[{{{}}([]"
                    "{<[[]]>}<{[{[{[]{()[[[]"
                    "[<(<(<(<{}))><([]([]()"
                    "<{([([[(<>()){}]>(<<{{"
                    "<{([{{}}[<[[[<>{}]]]>[]]"])

(def input-file (io/resource "day10/input.txt"))

(def real-input (str/split (slurp input-file) #"\n"))

;; frequencies is too naive, since it doesn't take order into account

;; conj-ing to a list behaves like a stack
;; peek gives top elem, pop gives rest
(pop (conj () 1 2 3))

(def opening-chars #{\( \{ \[ \<})
(def closing-chars #{\) \} \] \>})
;; brauch ich das?
(def pair-mapping {\( \) \{ \} \[ \] \< \>})

(def scores {\) 3 \} 1197 \] 57 \> 25137})

(defn is-opening-char?
  [c]
  (.contains opening-chars c))

(defn is-closing-char?
  [c]
  (.contains c closing-chars))

;; horrible abstraction
(defn offending-char-or-remaining-stack
  [s]
  (loop [stack ()
         remaining s
         offender nil]
    (let [next (first remaining)]
      (if (not (nil? offender))
        offender
        (if (empty? remaining)
          stack
          (if (is-opening-char? next)
            (recur (conj stack next) (rest remaining) offender)
            (cond
              (and (= next \)) (= (peek stack) \()) (recur (pop stack) (rest remaining) offender)
              (and (= next \}) (= (peek stack) \{)) (recur (pop stack) (rest remaining) offender)
              (and (= next \]) (= (peek stack) \[)) (recur (pop stack) (rest remaining) offender)
              (and (= next \>) (= (peek stack) \<)) (recur (pop stack) (rest remaining) offender)
              :else                                 (recur stack remaining next))))))))

(defn offending-chars
  [strs]
  (->> strs
       (map offending-char-or-remaining-stack)
       (filter char?)))

(defn remaining-stacks
  [strs]
  (->> strs
       (map offending-char-or-remaining-stack)
       (filter (comp not char?))))

;; part1
(let [input real-input]
  (->> input
       offending-chars
       (map scores)
       (reduce +))) ;; 339411

(def part2-scores {\( 1 \[ 2 \{ 3 \< 4})

(defn autocomplete-score
  [remaining-stack]
  (->> remaining-stack
       (map part2-scores)
       (reduce (fn [total, score] (-> total (* 5) (+ score))) 0)))

;; part2
(let [input real-input
      sorted-scores (->> input
                         remaining-stacks
                         (map autocomplete-score)
                         sort)]
  ;; conveniently, the number if incomplete lines is odd
  (nth sorted-scores (quot (count sorted-scores) 2))) ;; 2289754624
