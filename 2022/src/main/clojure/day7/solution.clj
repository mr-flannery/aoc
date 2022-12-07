(ns day7.solution
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]))

(def input (slurp (io/resource "day7/input.txt")))

(def sample-input "$ cd /\n$ ls\ndir a\n14848514 b.txt\n8504156 c.dat\ndir d\n$ cd a\n$ ls\ndir e\n29116 f\n2557 g\n62596 h.lst\n$ cd e\n$ ls\n584 i\n$ cd ..\n$ cd ..\n$ cd d\n$ ls\n4060174 j\n8033020 d.log\n5626152 d.ext\n7214296 k")

(def sample-commands (->> (str/split sample-input #"\$")
                          (filter seq)
                          (map str/trim)))

(def commands (->> (str/split input #"\$")
                   (filter seq)
                   (map str/trim)))

(defn cd
  [state cmd]
  (let [target-dir (second (re-find #"cd (.*)" cmd))]
    (cond
      (= target-dir "/") (assoc state :pwd [target-dir])
      (= target-dir "..") (update state :pwd #(->> % drop-last (into []))) ;turning vectors into seqs is always annoying
      :else (update state :pwd #(conj % target-dir)))))

(second (re-find #"cd (.*)" "cd /"))

(defn store-file
  [state line]
  (let [[dir-or-size name] (str/split line #" ")]
    (if (= dir-or-size "dir")
      (update-in state (concat [:fs] (:pwd state)) #(assoc % name {}))
      (update-in state (concat [:fs] (:pwd state)) #(assoc % name (Integer/parseInt dir-or-size))))))

(defn ls
  [state cmd]
  (let [output (drop 1 (str/split-lines cmd))]
    (reduce store-file state output)))

(defn execute-command
  [state cmd]
  (cond
    (str/starts-with? cmd "cd") (cd state cmd)
    (str/starts-with? cmd "ls") (ls state cmd)))

(def initial-state {:fs {"/" {}} :pwd ["/"]})

(println (-> initial-state :pwd first type))

(pprint sample-commands)

; okay jetzt erstmal was, was die reine dir-size berechnet
(defn dir-size
  [fs]
  (->> fs
       (map
         (fn [[k v]]
           (if (map? v)
             (dir-size v)
             v))
         )
       flatten
       (reduce +)))

(defn next-free-name
  [result name]
  (let [try (str name "1")]
    (if (contains? result try)
      (next-free-name result try)
      try)))

(defn blergi2
  [fs pwd]
  (loop [arg-stack [[pwd (get fs pwd)]]
         result {}]
    (if (empty? arg-stack)
      result
      (let [[pwd fs] (first arg-stack)

            files-size (->> (vals fs) (filter number?) (reduce +))
            dirs (->> fs (filter (fn [[k v]] (map? v))))
            dirs-size (->> dirs (map second) (map dir-size) (reduce +))]
        (recur (into [] (concat (rest arg-stack) dirs)) (if (contains? result pwd)
                                                          (do
                                                            (println "conflict")
                                                            (assoc result (next-free-name result pwd) (+ files-size dirs-size)))
                                                          (assoc result pwd (+ files-size dirs-size))))))))

(blergi2 {"/" {"a" 1 "b" 2 "c" {"a" 1 "b" 2 "d+" {"e" 1 "f" 2 "g" 3}}}} "/")

;; part1
(let [state (reduce execute-command initial-state commands)
      fs (:fs state)]
  (->> (blergi2 fs "/")
       (filter (fn [[k v]] (<= v 100000)))
       (map second)
       (reduce +)
       )
  )

;; part2
(let [state (reduce execute-command initial-state commands)
      fs (:fs state)
      target (- 30000000 (- 70000000 (dir-size fs)))
      ]
  target
  (->> (blergi2 fs "/")
       (map second)
       (filter #(>= % target))
       sort
       first
       )
  )
