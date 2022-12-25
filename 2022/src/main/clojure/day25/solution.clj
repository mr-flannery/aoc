(ns day25.solution
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.set :refer [intersection union]])
  (:import (java.util HashSet ArrayList)))

(def input (slurp (io/resource "day25/input.txt")))

(def sample-input "1=-0-2\n12111\n2=0=\n21\n2=01\n111\n20012\n112\n1=-1=\n1-12\n12\n1=\n122")

(defn parse
  [input]
  (str/split-lines input))

(def sample-snafus (parse sample-input))
(def real-snafus (parse input))

(def s-dig->d-dig {\= -2
                   \- -1
                   \0 0
                   \1 1
                   \2 2})

(def q-dig->d-dig {\0 0
                   \1 1
                   \2 2
                   \3 3
                   \4 4})

(defn ->decimal
  [dig-fn input]
  (loop [[digit & ds] (reverse input)
         exp    0
         result 0]
    (if (nil? digit)
      (.longValue result)
      (let [new (* (dig-fn digit) (Math/pow 5 exp))]
        (recur ds (inc exp) (+ result new))))))

(def snafu->decimal (partial ->decimal s-dig->d-dig))
(def quint->decimal (partial ->decimal q-dig->d-dig))

; the offset between two same-length quint and snafu numbers with the same "respective-valued" digits is always the (snafu (repeat length 2))
(= (snafu->decimal "22") (- (quint->decimal "40") (snafu->decimal "2=")))
(= (snafu->decimal "222") (- (quint->decimal "401") (snafu->decimal "2=-")))

; this must somehow be useful

(defn required-digits-snafu
  [decimal]
  (loop [digs 1]
    (if (>= (snafu->decimal (apply str (repeat digs "2"))) decimal)
      digs
      (recur (inc digs)))))

(defn required-digits-quint
  [decimal]
  (loop [digs 1]
    (if (>= (quint->decimal (apply str (repeat digs "4"))) decimal)
      digs
      (recur (inc digs)))))

(defn max-snafu-for-digs->decimal
  [n]
  (snafu->decimal (apply str (repeat n "2"))))

(defn decimal->quint
  [decimal]
  (loop [remainder        decimal
         res              ""
         remaining-digits (required-digits-quint remainder)]
    (println remainder)
    (if (= remainder 0)
      (apply str res (repeat remaining-digits "0"))
      (let [next-digit (->> [4 3 2 1 0]
                            (drop-while #(> (* % (Math/pow 5 (dec remaining-digits))) remainder))
                            first)
            dig-val    (* next-digit (Math/pow 5 (dec remaining-digits)))]
        (recur (.longValue (- remainder dig-val)) (str res next-digit) (dec remaining-digits))))))

(def q-dig->s-dig {\4 \2
                   \3 \1
                   \2 \0
                   \1 \-
                   \0 \=})

(defn decimal->snafu
  [decimal]
  (let [req-s  (required-digits-snafu decimal)
        offset (max-snafu-for-digs->decimal req-s)]
    (->> (decimal->quint (+ decimal offset))
         (map q-dig->s-dig)
         (apply str))))

;part1
(let [snafus real-snafus]
  (->> snafus
       (map snafu->decimal)
       (reduce +)
       decimal->snafu
       ))
