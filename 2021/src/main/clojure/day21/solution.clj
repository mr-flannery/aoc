(ns day21
  (:require [clojure.pprint :refer [pprint]]))

(def example-game-state {:1 {:position 4 :score 0} :2 {:position 8 :score 0} :rolls 0})
(def real-game-state {:1 {:position 4 :score 0} :2 {:position 3 :score 0} :rolls 0})

(defn next-position
  [start move-by]
  (->> (cycle (range 1 11))
       (drop (dec (+ start move-by)))
       first))

(defn practice-rolls-for-turn
  [n]
  (->> (cycle (range 1 101))
       (drop (* (dec n) 3))
       (take 3)))

(defn turn
  [game-state turn-count]
  (let [current-player                    (if (odd? turn-count) 1 2)
        {position :position score :score} (game-state (keyword (str current-player)))
        move-by                           (reduce + (practice-rolls-for-turn turn-count))
        next-pos                          (next-position position move-by)]
    (-> game-state
        (assoc-in [(keyword (str current-player)) :position] next-pos)
        (update-in [(keyword (str current-player)) :score] #(+ % next-pos))
        (update-in [:rolls] #(+ % 3)))))

(defn play-game
  [start-state]
  (loop [game-state  start-state
         turn-count  1]
    (if (or (>= ((game-state :1) :score) 1000) (>= ((game-state :2) :score) 1000))
      game-state
      (recur (turn game-state turn-count) (inc turn-count)))))

;; part1
(let [{{p1-score :score} :1
       {p2-score :score} :2
       rolls :rolls}        (play-game real-game-state)]
  (* rolls (if (< p1-score p2-score) p1-score p2-score))) ;; 734820

(defn turn2
  [game-state turn-count]
  (for [d1 (range 1 4)
        d2 (range 1 4)
        d3 (range 1 4)]
    (let [current-player                    (if (odd? turn-count) 1 2)
          {position :position score :score} (game-state (keyword (str current-player)))
          move-by                           (+ d1 d2 d3)
          next-pos                          (next-position position move-by)]
      (-> game-state
          (assoc-in [(keyword (str current-player)) :position] next-pos)
          (update-in [(keyword (str current-player)) :score] #(+ % next-pos))
          (update-in [:rolls] inc)))))

(defn is-finished?
  [[{{score1 :score} :1 {score2 :score} :2}]]
  (or (>= score1 21) (>= score2 21))) ;; todo 21

(defn winner
  [[{{score1 :score} :1 {score2 :score} :2}]]
  (if (> score1 score2) :1 :2))

;; that's the naive approach
(defn play-game2
  [game-state]
  (loop [game-states    [game-state]
         finished-games []
         turn-count     1]
    (if (empty? game-states)
      finished-games
      (let [{finished true running false} (group-by is-finished? game-states)]
        (recur (mapcat #(turn2 % turn-count) running) (concat finished-games finished) (inc turn-count))))))

(comment 
  (->> (turn2 example-game-state 1)
       (group-by identity)
       (map (fn [[k v]] [k (* 3 (count v))]))
       (into {})
       pprint))

(defn play-game3
  [game-state]
  (loop [game-states    {game-state 1}
         finished-games {}
         turn-count     1]
    (if (empty? game-states)
      finished-games
      (let [{finished true running false} (group-by is-finished? game-states)
            next-game-states              (for [[game-state state-count] running]
                                            (->> (turn2 game-state turn-count)
                                                 (group-by identity)
                                                 (map (fn [[k v]] [k (* state-count (count v))]))
                                                 (into {})))]
        (recur 
         (apply merge-with + next-game-states) 
         (merge-with + finished-games (into {} finished))
         (inc turn-count))))))

(let [{p1-states :1 p2-states :2} (->> (play-game3 real-game-state)
                                       (group-by winner))]
  (apply max [(reduce + (vals p1-states)) (reduce + (vals p2-states))]))