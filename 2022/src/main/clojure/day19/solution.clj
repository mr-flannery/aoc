(ns day19.solution
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.set :refer [intersection union]])
  (:import (java.util HashSet ArrayList)))

(def input (slurp (io/resource "day19/input.txt")))

(def sample-input "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.\nBlueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.")

(defn parse
  [input]
  (->> (str/split-lines input)
       (map #(re-matches #".* (\d+):.*(\d+) ore.*(\d+) ore.*(\d+) ore and (\d+) clay.*(\d+) ore and (\d+) obsidian." %))
       (map #(drop 1 %))
       (map (fn [col] (map #(Integer/parseInt %) col)))
       (map (fn [[id ore-ore clay-ore obsidian-ore obsidian-clay geode-ore geode-obsidian]]
              {:id id :o-o ore-ore :c-o clay-ore :ob-o obsidian-ore :ob-c obsidian-clay :g-o geode-ore :g-ob geode-obsidian}))))

(def sample-blueprints (parse sample-input))

(defn options
  [blueprint state]
  (let [opts (ArrayList.)]
    (if (and (>= (state :ore) (blueprint :g-o)) (>= (state :obsidian) (blueprint :g-ob)))
      (.add opts :geode-robot)
      (do
        (if (and (>= (state :ore) (blueprint :ob-o)) (>= (state :clay) (blueprint :ob-c)))
          (.add opts :obsidian-robot))
        (if (>= (state :ore) (blueprint :c-o))
          (.add opts :clay-robot))
        (if (>= (state :ore) (blueprint :o-o))
          (.add opts :ore-robot))
        (.add opts :none)))
    opts))

(defn build-robot
  [blueprint state action]
  (condp = action
    :ore-robot (-> state (update :ore #(- % (blueprint :o-o))) (update :ore-robots inc))
    :clay-robot (-> state (update :ore #(- % (blueprint :c-o))) (update :clay-robots inc))
    :obsidian-robot (-> state (update :ore #(- % (blueprint :ob-o))) (update :clay #(- % (blueprint :ob-c))) (update :obsidian-robots inc))
    :geode-robot (-> state (update :ore #(- % (blueprint :g-o))) (update :obsidian #(- % (blueprint :g-ob))) (update :geode-robots inc))
    :none state))

(defn collect-resources
  [state]
  (-> state
      (update :ore #(+ % (state :ore-robots)))
      (update :clay #(+ % (state :clay-robots)))
      (update :obsidian #(+ % (state :obsidian-robots)))
      (update :geode #(+ % (state :geode-robots)))))

(def initial-state {:ore 0 :ore-robots 1 :clay 0 :clay-robots 0 :obsidian 0 :obsidian-robots 0 :geode 0 :geode-robots 0})

(defn next-states
  [blueprint state turn]
  (->> (options blueprint state)
       (map #(build-robot blueprint (collect-resources state) %))
       (map (fn [state] [state (inc turn)]))))

(comment
  (time
    (let [blueprint                 (first sample-blueprints)
          max-geodes                (atom 0)
          turns                     24
          max-geode-robots-per-turn (atom (zipmap (range 0 24) (repeat 24 0)))]
      (loop [[[state turn] & states] [[initial-state 0]]]
        ;(println turn)
        (if (nil? state)
          @max-geodes
          (if (= turn turns)
            (do
              ;(println state)
              (swap! max-geodes #(max % (state :geode)))
              (recur states))
            (if (> (get @max-geode-robots-per-turn turn) (state :geode-robots))
              (recur states)
              (do
                ;(println "found " (get @max-geode-robots-per-turn turn) ", update with " (state :geode-robots))
                (swap! max-geode-robots-per-turn #(assoc % turn (state :geode-robots)))
                ; TODO: unclear whether this makes an appreciable difference in the end
                ;(recur (into (into [] states) (next-states blueprint state turn)))
                (recur (into (next-states blueprint state turn) states))))))))))

; can I somehow modify my search so that I prioritize states that arrive early at the next higher robot?
; okay, so like this I get the quickest way to 1 clay robot
(def target-seq (concat [[:clay-robots 1] [:obsidian-robots 1]] (iterate (fn [[robot count]] [robot (inc count)]) [:geode-robots 1])))

(comment
  (time
    (let [blueprint          (second sample-blueprints)
          turns              24
          target-seq-counter (atom 0)
          next-target        (fn []
                               (let [res (nth target-seq @target-seq-counter)]
                                 (swap! target-seq-counter inc)
                                 res))
          result             (atom #{})]
      (loop [[[state turn] & states] [[initial-state 0]]
             next-starting-state nil
             [target-robot target-number] (next-target)]
        ;(println (count states))
        ;(println [state turn] next-starting-state)
        (if (= turn turns) ; state is played out
          (do
            (swap! result #(conj % [state turn])) ; add the state to the result set
            (recur states next-starting-state [target-robot target-number]))
          (if (nil? state) ; we considered all relevant states
            (if (some? next-starting-state) ; we reached the next relevant target and have a new jumping-off point
              (do
                (reset! result #{}) ; we clear the result set so that we only keep the states from the last jumping-off point in the end
                (recur [next-starting-state] nil (next-target)))
              @result) ; or we're just done
            (if (= target-number (target-robot state)) ; we fulfill the condition for reaching the next target
              (if (and next-starting-state (< (second next-starting-state) turn))
                (recur states next-starting-state [target-robot target-number]) ; if we have a candidate already and it took less turn to get there, keep the current one
                (recur states [state turn] [target-robot target-number])) ; otherwise use the current state as a better candidate
              (if (>= turn (or (second next-starting-state) 24)) ; check if we've exceeded the best current candidate already
                (recur states next-starting-state [target-robot target-number])
                (recur (into (next-states blueprint state turn) states) next-starting-state [target-robot target-number]))))))))) ; if not build new states from the current one

(take 10 (concat [[:clay-robot 1] [:obsidian-robot 1]] (iterate (fn [[robot count]] [robot (inc count)]) [:geode-robot 1])))

(defn nats-from-n
  [n]
  (lazy-cat [n] (nats-from-n (inc n))))

(def nats-from-1 (nats-from-n 1))

(let [blueprint  (first sample-blueprints)
      clay-costs {:ore 2}
      ore-robots 1]
  (/ (clay-costs :ore) ore-robots))

; was sind die kosten um zu n ore-robots zu kommen, gegeben einem gewissen state
; ich kan

; je mehr ich drüber nachdenke, desto mehr könnte der DP ansatz doch quatsch sein
; zumindest mal weil der state mitreinspielt
(def cost-for-n-ore-robots
  "[blueprint state n]"
  (memoize
    (fn cost-for-n-ore-robots
      [blueprint state n]
      (let [{:keys [o-o]} blueprint
            {:keys [ore ore-robots]} state
            target n
            diff   (- target ore-robots)]
        (if (= diff 1)
          (let [turns (.intValue (Math/ceil (inc (/ (- o-o ore) ore-robots))))]
            [{:ore        (- (+ ore (* turns ore-robots)) o-o)
              :ore-robots (inc ore-robots)}
             turns])
          (let [[new-state turns] (cost-for-n-ore-robots blueprint state diff)
                new-state (merge state new-state)
                {:keys [ore ore-robots]} new-state
                new-turns (.intValue (Math/ceil (inc (/ (max 0 (- o-o ore)) ore-robots))))]
            [(-> new-state
                 (assoc :ore (- (+ ore (* new-turns ore-robots)) o-o))
                 (update :ore-robots inc))
             (+ turns new-turns)]))))))

;kann ich mir irgendwie Ziele setzen?
;ich weiß, dass ich erstmal zu nem clay robot will
; das kostet mich x ore
; also ist die frage wie ich am schnellsten nach x ore komme
; wenn ich den habe, will ich nen obisidian robot
; der kostet mich x ore und y clay
; so jetzt habe ich zu diesem Zeitpunkt schon 
