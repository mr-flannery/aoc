(ns day8
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def example-input [[["be" "cfbegad" "cbdgef" "fgaecd" "cgeb" "fdcge" "agebfd" "fecdb" "fabcd" "edb"]
                     ["fdgacbe" "cefdb" "cefbgd" "gcbe"]]
                    [["edbfga" "begcd" "cbg" "gc" "gcadebf" "fbgde" "acbgfd" "abcde" "gfcbed" "gfec"]
                     ["fcgedb" "cgb" "dgebacf" "gc"]]
                    [["fgaebd" "cg" "bdaec" "gdafb" "agbcfd" "gdcbef" "bgcad" "gfac" "gcb" "cdgabef"]
                     ["cg" "cg" "fdcagb" "cbg"]]
                    [["fbegcd" "cbd" "adcefb" "dageb" "afcb" "bc" "aefdc" "ecdab" "fgdeca" "fcdbega"]
                     ["efabcd" "cedba" "gadfec" "cb"]]
                    [["aecbfdg" "fbg" "gf" "bafeg" "dbefa" "fcge" "gcbea" "fcaegb" "dgceab" "fcbdga"]
                     ["gecf" "egdcabf" "bgf" "bfgea"]]
                    [["fgeab" "ca" "afcebg" "bdacfeg" "cfaedg" "gcfdb" "baec" "bfadeg" "bafgc" "acf"]
                     ["gebdcfa" "ecba" "ca" "fadegcb"]]
                    [["dbcfg" "fgd" "bdegcaf" "fgec" "aegbdf" "ecdfab" "fbedc" "dacgb" "gdcebf" "gf"]
                     ["cefg" "dcbef" "fcge" "gbcadfe"]]
                    [["bdfegc" "cbegaf" "gecbf" "dfcage" "bdacg" "ed" "bedf" "ced" "adcbefg" "gebcd"]
                     ["ed" "bcgafe" "cdgba" "cbgef"]]
                    [["egadfb" "cdbfeg" "cegd" "fecab" "cgb" "gbdefca" "cg" "fgcdab" "egfdb" "bfceg"]
                     ["gbdfcae" "bgc" "cg" "cgb"]]
                    [["gcafb" "gcf" "dcaebfg" "ecagb" "gf" "abcdeg" "gaef" "cafbge" "fdbac" "fegbdc"]
                     ["fgae" "cfgab" "fg" "bagce"]]])

(def input-file (io/resource "day8/input.txt"))

(def real-input (->> (-> (slurp input-file)
                         (str/split #"\n"))
                     (mapcat #(str/split % #" \| "))
                     (map #(str/split % #" "))
                     (partition 2)))

(->> example-input
     (map first)
     (map #(group-by count %))
     (map #(map (fn [[k v]] [k (count v)]) %)))

;; part 1
(->> real-input
     (map second)
     (map #(group-by count %))
     (mapcat identity)
     (filter (fn [[count _]] (.contains [2 3 4 7] count)))
     (map second)
     (map count)
     (reduce +)) ;; 416

(defn segment-contains?
  "Does s1 segment-contain s2?
   For example, the segment-string for 3 segment-contains the segment-string for 7, since three uses all segments that 7 is using."
  [s1 s2]
  (clojure.set/superset? (set s1) (set s2)))

(defn first-round
  [seg-strings]
  (reduce
   (fn [m seg-string]
     (cond
       (= (count seg-string) 2) (assoc m :1 seg-string)
       (= (count seg-string) 3) (assoc m :7 seg-string)
       (= (count seg-string) 4) (assoc m :4 seg-string)
       (= (count seg-string) 7) (assoc m :8 seg-string)
       :else m))
   {}
   seg-strings))

(first-round (first (first example-input)))

(defn three
  [seg-strings seven]
  (first (filter #(and (= (count %) 5) (segment-contains? % seven)) seg-strings)))

(defn nine
  [seg-strings seven four]
  (first (filter #(and
                   (= (count %) 6)
                   (segment-contains? % seven)
                   (segment-contains? % four)) seg-strings)))

;; 0 => 6 segments and superset of 7
(defn zero
  [seg-strings seven]
  (first (filter #(and (= (count %) 6) (segment-contains? % seven)) seg-strings)))

;; 5 => 5 segments and subset of 9
(defn five
  [seg-strings nine]
  (first (filter #(and (= (count %) 5) (segment-contains? nine %)) seg-strings)))

;; 6 => 6 segments and superset of 5
(defn six
  [seg-strings five]
  (first (filter #(and (= (count %) 6) (segment-contains? % five)) seg-strings)))

;; 2 => remaining

(defn decode
  [seg-strings]
  (let [{one :1 four :4 seven :7 eight :8} (first-round seg-strings)
        rest (remove #{one four seven eight} seg-strings)
        three (three rest seven)
        nine (nine rest seven four)
        rest (remove #{three nine} rest)
        zero (zero rest seven)
        five (five rest nine)
        rest (remove #{zero five} rest)
        six (six rest five)
        two (first (remove #{six} rest))]
    {zero "0" one "1" two "2" three "3" four "4" five "5" six "6" seven "7" eight "8" nine "9"}))

(defn decoded-number
  [mapping digits]
  (map #(get mapping %) digits))

(defn sorted-input
  [input]
  (->> input
     (map (fn [[seg-strings digits]] [(->> seg-strings
                                           (map sort)
                                           (map #(apply str %)))
                                      (->> digits
                                           (map sort)
                                           (map #(apply str %)))]))))
;; part2 
(->> (sorted-input real-input)
     (map (fn [[seg-strings digits]] (decoded-number (decode seg-strings) digits)))
     (map #(apply str %))
     (map #(Integer/parseInt %))
     (reduce +)) ;; 1043697

;; code quality of this solution is under all pig
