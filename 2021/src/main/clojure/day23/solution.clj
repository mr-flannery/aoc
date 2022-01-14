(ns day23
  (:require [clojure.pprint :refer [pprint]]))

(def example-input ["#############"
                    "#...........#"
                    "###B#C#B#D###"
                    "  #A#D#C#A#  "
                    "  #########  "])

(def state {[0 0]  {:pod :empty}
            [1 0]  {:pod :empty}
            [2 0]  {:pod :empty}
            [3 0]  {:pod :empty}
            [4 0]  {:pod :empty}
            [5 0]  {:pod :empty}
            [6 0]  {:pod :empty}
            [7 0]  {:pod :empty}
            [8 0]  {:pod :empty}
            [9 0]  {:pod :empty}
            [10 0] {:pod :empty}
            [2 1]  {:pod :B :has-moved false}
            [2 2]  {:pod :A :has-moved false}
            [4 1]  {:pod :C :has-moved false}
            [4 2]  {:pod :D :has-moved false}
            [6 1]  {:pod :B :has-moved false}
            [6 2]  {:pod :C :has-moved false}
            [8 1]  {:pod :D :has-moved false}
            [8 2]  {:pod :A :has-moved false}
            :moves []
            :cost  0})

(comment
  "#############"
  "#...........#"
  "###D#B#A#C###"
  "  #C#A#D#B#  "
  "  #########  ")

(def real-input {[0 0]  {:pod :empty}
                 [1 0]  {:pod :empty}
                 [2 0]  {:pod :empty}
                 [3 0]  {:pod :empty}
                 [4 0]  {:pod :empty}
                 [5 0]  {:pod :empty}
                 [6 0]  {:pod :empty}
                 [7 0]  {:pod :empty}
                 [8 0]  {:pod :empty}
                 [9 0]  {:pod :empty}
                 [10 0] {:pod :empty}
                 [2 1]  {:pod :D :has-moved false}
                 [2 2]  {:pod :C :has-moved false}
                 [4 1]  {:pod :B :has-moved false}
                 [4 2]  {:pod :A :has-moved false}
                 [6 1]  {:pod :A :has-moved false}
                 [6 2]  {:pod :D :has-moved false}
                 [8 1]  {:pod :C :has-moved false}
                 [8 2]  {:pod :B :has-moved false}
                 :moves []
                 :cost  0})

(def cost {:A 1
           :B 10
           :C 100
           :D 1000})

(defn hallway-pos-beetween
  [x1 x2]
  (cond
    (< x1 x2) (for [x (range (inc x1) (inc x2))] [x 0])
    (> x1 x2) (for [x (range (dec x1) (dec x2) -1)] [x 0])
    :else     []))

(defn is-empty?
  [state [x y]]
  (= :empty (:pod (get state [x y]))))

(defn possible-moves
  [[x y] pod state]
  (if (= y 2) ;; back room pos
    (if (is-empty? state [x 1])
      (->> (concat
            (take-while #(is-empty? state %) (for [x1 (range (inc x) 11)] [x1 0]))
            (take-while #(is-empty? state %) (for [x1 (range (dec x) -1 -1)] [x1 0])))
           (filter (fn [[x y]] (not (.contains [2 4 6 8] x)))))
      [])
    (if (= y 1) ;; front room pos
      (->> (concat
            (take-while #(is-empty? state %) (for [x1 (range (inc x) 11)] [x1 0]))
            (take-while #(is-empty? state %) (for [x1 (range (dec x) -1 -1)] [x1 0])))
           (filter (fn [[x y]] (not (.contains [2 4 6 8] x)))))
      ;; hallway pos
      (->> (concat
            (->> (concat ;; hallway moves (ignoring usefulness)
                  (take-while #(is-empty? state %) (for [x1 (range (inc x) 11)] [x1 0]))
                  (take-while #(is-empty? state %) (for [x1 (range (dec x) -1 -1)] [x1 0])))
                 (filter (fn [[x y]] (not (.contains [2 4 6 8] x)))))
            (for [x1 [2 4 6 8]]
              (if (and (is-empty? state [x1 2]) (every? #(is-empty? state %) (hallway-pos-beetween x x1)))
                [x1 2]
                (do
                  ;; (println x1)
                  ;; (println pod)
                  ;; (println (= pod (let [temp (get state [x1 2])] (if (seq? temp) (do (println (first temp))(first temp)) :empty))))
                  ;; (println (every? #(is-empty? state %) (hallway-pos-beetween x x1)))
                  ;; (println)
                  (if (and
                       (is-empty? state [x1 1])
                       (= pod ((get state [x1 2]) :pod))
                       (every? #(is-empty? state %) (hallway-pos-beetween x x1)))
                    [x1 1])))))
           (filter some?)))))

(possible-moves [2 1] :B state)

(let [state {[0 0]  {:pod :empty}
             [1 0]  {:pod :empty}
             [2 0]  {:pod :empty}
             [3 0]  {:pod :d :has-moved true}
             [4 0]  {:pod :empty}
             [5 0]  {:pod :A :has-moved true}
             [6 0]  {:pod :empty}
             [7 0]  {:pod :B :has-moved true}
             [8 0]  {:pod :empty}
             [9 0]  {:pod :empty}
             [10 0] {:pod :empty}
             [2 1]  {:pod :empty}
             [2 2]  {:pod :A :has-moved false}
             [4 1]  {:pod :empty}
             [4 2]  {:pod :empty}
             [6 1]  {:pod :empty}
             [6 2]  {:pod :D :has-moved false}
             [8 1]  {:pod :empty}
             [8 2]  {:pod :B :has-moved false}}]
  (possible-moves [7 0] :B state))

;; (defn is-blocked?
;;   []
;;   ())

;; (defn can-move?
;;   []
;;   (and 
;;    (not has-moved) 
;;    (not (is-blocked?))))

(for [[[x y] {:keys [pod has-moved]}] (filter (fn [[_ {pod :pod}]] (not= pod :empty)) state)]
  (possible-moves [x y] pod state))

;; was ist denn eigentlich der game plan?
;; schritt 1: wir müssen einen raum so frei machen, dass ein andere pod da reinmoven kann
;; d.h. wir suchen einen raum, in dem ein pod vorne steht und einen raum, in dem der gleiche pod hinten steht

(def roompairs [[2 4] [2 6] [2 8] [4 6] [4 8] [6 8]])
(def front-of-room [[2 0] [4 0] [6 0] [8 0]])

(defn openings
  [state]
  (->> (for [[room1 room2] roompairs]
         (cond
           (= ((get state [room1 1]) :pod) ((get state [room2 2]) :pod)) [room1 room2] ;; room2 needs to free up
           (= ((get state [room1 2]) :pod) ((get state [room2 1]) :pod)) [room2 room1]))
       (filter some?)))

;; wait, could this already work for right and left?
(defn target-fields-right
  [state x]
  (->> state
       (filter (fn [[k v]] (.contains (hallway-pos-beetween x 10) k)))
       (filter (fn [[k _]] (not (.contains front-of-room k))))
       sort))

(defn target-fields-left
  [state x]
  (->> state
       (filter (fn [[k v]] (.contains (hallway-pos-beetween x 0) k)))
       (filter (fn [[k _]] (not (.contains front-of-room k))))
       (sort (fn [[[x1]] [[x2]]] (> x1 x2)))))

(defn contains-empty-field?
  [pos-seq]
  (some (fn [[_ {pod :pod}]] (= pod :empty)) pos-seq))

(defn free-first-target-field
  [target-fields]
  (if (contains-empty-field? target-fields)
    (->> target-fields
         (reduce (fn [result pos]
                   (if (contains-empty-field? result)
                     result
                     (conj result pos))) [])
         (partition 2 1)
         reverse
         (map (fn [[[k1 _] [k2 _]]] [k1 k2])))))

;; this function should be made obsolete by a more general implemention of free-hallway-from-to-moves
(defn free-front-pos-moves
  [state [from-room to-room]]
  (let [target-fields ((if (> to-room from-room) target-fields-right target-fields-left) state to-room)]
    (if (= :empty ((second (first target-fields)) :pod))
      [[to-room 1] (first (first target-fields))]
          ;; if the first field is not free, check if I can make space by moving other pods
      (free-first-target-field target-fields))))

(defn abs
  [n]
  (max n (- n)))

(defn free-hallway-from-to-moves
  [state [from to]]
  (let [target-fields    ((if (> to from) target-fields-right target-fields-left) state from)
        blocking-pods    (count (filter (fn [pos] (not= :empty ((get state pos) :pod))) (hallway-pos-beetween from to)))
        num-pods         (count (filter (fn [[_ {pod :pod}]] (not= :empty pod)) target-fields))
        num-empty-fields (- (count target-fields) num-pods)]
    (if (= 0 blocking-pods)
      []
      (if (= 1 blocking-pods) ;; let's just make the assumption here that I will have at most one pod in the hallway
        (let [[x y] (first (first (filter (fn [[_ {pod :pod}]] (not= :empty pod)) target-fields)))]
          (if (> (abs (- x from)) (abs (- x to)))
            [[x y] [(dec to) 0]]
            [[x y] [(inc from) 0]]))
        (println "ASSUMPTION VIOLATED!!")))))
    ;; (if (contains-empty-field? target-fields)
    ;;   (->> target-fields
    ;;        (reduce (fn [result pos]
    ;;                  (if (contains-empty-field? result)
    ;;                    result
    ;;                    (conj result pos))) [])
    ;;        (partition 2 1)
    ;;        reverse
    ;;        (map (fn [[[k1 _] [k2 _]]] [k1 k2]))))


;; das ist hier eigentlich quark, ich muss mir den kompletten hallway angucken
;; ich kann in der theorie ja sowohl nach links als auch nach rechts ausweichen
;; ich kann glaube ich auch einfach annehmen, dass
(let [state {[0 0]  {:pod :empty}
             [1 0]  {:pod :empty}
             [2 0]  {:pod :empty}
             [3 0]  {:pod :B :has-moved false}
             [4 0]  {:pod :empty}
             [5 0]  {:pod :B :has-moved false}
             [6 0]  {:pod :empty}
             [7 0]  {:pod :empty}
             [8 0]  {:pod :empty}
             [9 0]  {:pod :B :has-moved false}
             [10 0] {:pod :empty}
             [2 1]  {:pod :B :has-moved false}
             [2 2]  {:pod :A :has-moved false}
             [4 1]  {:pod :C :has-moved false}
             [4 2]  {:pod :D :has-moved false}
             [6 1]  {:pod :B :has-moved false}
             [6 2]  {:pod :C :has-moved false}
             [8 1]  {:pod :D :has-moved false}
             [8 2]  {:pod :A :has-moved false}}]
  (free-hallway-from-to-moves state [4 8]))

(defn opening-moves
  [state]
  (let [openings (openings state)]
    (for [[from-room to-room] openings]
      (let [free-front-pos-moves (free-front-pos-moves state [from-room to-room])
            move-to-target-room-move [[from-room 1] [to-room 1]]]
        (if (every? #(is-empty? state %) (hallway-pos-beetween from-room to-room))
          [free-front-pos-moves move-to-target-room-move]
          (conj [] (free-hallway-from-to-moves state [from-room to-room]) free-front-pos-moves move-to-target-room-move))))))

(defn from-to-moves
  [state [from-x from-y] to-room]
  (if (= 1 from-y)
    (let [;; free-front-pos-moves (free-front-pos-moves state [from-room to-room])
          move-to-target-room-move [[from-x 1] [to-room 1]]]
      (if (every? #(is-empty? state %) (hallway-pos-beetween from-x to-room))
        [move-to-target-room-move]
        (conj [] (free-hallway-from-to-moves state [from-x to-room]) move-to-target-room-move)))
    (let [free-front-pos-move (if (> to-room from-x) [[from-x 1] [(dec from-x) 0]] [[from-x 1] [(inc from-x) 0]])
          move-to-target-room-move [[from-x 2] [to-room 1]]]
      (println free-front-pos-move)
      (if (every? #(is-empty? state %) (hallway-pos-beetween from-x to-room))
        [free-front-pos-move move-to-target-room-move]
        (conj [] (free-hallway-from-to-moves state [from-x to-room]) free-front-pos-move move-to-target-room-move)))))

(defn move-cost
  [pod [from-x from-y] [to-x to-y]]
  (* (cost pod) (+ (abs (- from-x to-x)) (abs (- from-y to-y)))))

(defn apply-move
  [state [from to]]
  (let [{:keys [pod has-moved]} (get state from)]
    (-> state
        (assoc to {:pod pod :has-moved true})
        (assoc from {:pod :empty})
        (update :moves #(conj % [from to]))
        (update :cost #(+ % (move-cost pod from to))))))

(defn reverse-move
  [state]
  (let [[to from] (last (state :moves))
        {:keys [pod has-moved]} (get state from)]
    (-> state
        (assoc to {:pod pod :has-moved true})
        (assoc from {:pod :empty})
        (update :moves #(butlast %))
        (update :cost #(- % (move-cost pod from to))))))

(defn apply-moves
  [state moves]
  (reduce apply-move state moves))

(pprint (apply-moves state [[[6 1] [7 0]] [[4 1] [6 1]]]))

;; check which state the board is in
(defn is-finished?
  [state]
  (every? identity (for [room [2 4 6 8]]
                     (and
                      (=
                       ((get state [room 1]) :pod)
                       ((get state [room 2]) :pod))
                      (not=
                       ((get state [room 1]) :pod)
                       :empty)))))

(defn is-hallway-empty?
  [state]
  (every? (fn [[_ {pod :pod}]] (= pod :empty)) (hallway state)))

(defn hw-pod-count
  [state]
  (->> (hallway-pos-beetween -1 10)
       (map #(get state %))
       (filter (fn [{pod :pod}] (not= pod :empty)))
       count))

(defn is-1-pod-in-hw?
  [state]
  (= 1 (hw-pod-count state)))

(def pod->char {:A     \A
                :B     \B
                :C     \C
                :D     \D
                :empty \.})

(defn print-state
  [state]
  (doseq [line [(apply str (repeat 13 "#"))
                (str "#" (apply str (->> (hallway-pos-beetween -1 10)
                                         (map #(get state %))
                                         (map :pod)
                                         (map pod->char)))
                     "#")
                (apply str ["###" (pod->char (:pod (get state [2 1]))) "#" (pod->char (:pod (get state [4 1]))) "#" (pod->char (:pod (get state [6 1]))) "#" (pod->char (:pod (get state [8 1]))) "###"])
                (apply str ["  #" (pod->char (:pod (get state [2 2]))) "#" (pod->char (:pod (get state [4 2]))) "#" (pod->char (:pod (get state [6 2]))) "#" (pod->char (:pod (get state [8 2]))) "# "])
                "  #########  "]]
    (println line)))

(print-state state)

(defn hallway
  [state]
  (->> (hallway-pos-beetween -1 10)
       (map (fn [pos] [pos (get state pos)]))))

(defn room
  [state x]
  (select-keys state [[x 1] [x 2]]))

(defn rooms
  [state]
  (select-keys state [[2 1] [2 2] [4 1] [4 2] [6 1] [6 2] [8 1] [8 2]]))


(defn free-back-pos-moves
  [state room-x hx]
  (let [target-fields ((if (> hx room-x) target-fields-left target-fields-right) state room-x)]
    (if (= :empty ((second (first target-fields)) :pod))
      [[room-x 2] (first (first target-fields))]
          ;; if the first field is not free, check if I can make space by moving other pods
      (free-first-target-field target-fields))))


(defn in-process-moves
  [state]
  (let [[[hx hy] {pod :pod}] (->> (hallway state)
                           (filter (fn [[_ {pod :pod}]] (not= pod :empty)))
                           first)
        target-room (->> (for [x [2 4 6 8]] (room state x))
                         (filter (fn [room] (or (= :empty (-> room first second :pod)) (= :empty (-> room second second :pod)))))
                         first)
        room-pod (-> target-room second second :pod)
        room-pod-has-moved (-> target-room second second :has-moved)
        room-x (-> target-room first first first)]
    (println target-room)
    (println room-pod-has-moved)
    (cond
      ;; we can just move into the room
      (= pod room-pod) [[[hx hy] [room-x 1]]]
      ;; room-pod has moved, so we cannot move it out, but find the other one instead
      room-pod-has-moved (let [[[from-x from-y]] (->> (rooms state)
                                               (filter (fn [[_ {pod :pod hm :has-moved}]] (and (not hm) (= pod room-pod))))
                                               first)]
                           (println from-x from-y)
                           (from-to-moves state [from-x from-y] room-x))
      ;; we need to clear the room and move the hw pod into pos2
      :else (conj [] (free-back-pos-moves state room-x hx) [[hx hy] [room-x 2]]))))

(let [state1 state
      state2 (apply-moves state1 (first (opening-moves state1)))
      state3 (apply-moves state2 (in-process-moves state2))
      state4 (apply-moves state3 (in-process-moves state3))
      state5 (apply-moves state4 (in-process-moves state4))
      state6 (apply-moves state5 (in-process-moves state5))
      state7 (apply-moves state6 (in-process-moves state6))]
  state7)

(defn two-hw-moves 
  [state]
  ;; assumption: one room is completely empty
  (let [hw-pods (filter (fn [[_ {pod :pod}]] (not= pod :empty)) (hallway state))
        empty-room-x (->> (rooms state)
                          (partition 2)
                          (filter (fn [[[_ {pod :pod} _]]] (= pod :empty))) ;; asumption: one room will be completely empty
                          first
                          first
                          first
                          first)]
    (if (= (-> hw-pods first second :pod) (-> hw-pods second second :pod)) 
        (let [[[x1 y1]] (first (sort-by (fn [[_ {pod :pod}]] pod) hw-pods))
              [[x2 y2]] (second (sort-by (fn [[_ {pod :pod}]] pod) hw-pods))]
          (println "finisher?")
          [[[x1 y1] [empty-room-x 2]] [[x2 y2] [empty-room-x 1]]])
        (let [[[lc-pod-x lc-pod-y]] (first (sort-by (fn [[_ {pod :pod}]] pod) hw-pods))]
          [[[lc-pod-x lc-pod-y] [empty-room-x 2]]]))))

(defn run-simulation
  [state]
  (loop [queue [state]
         done  []
         limit 0]
    (if (or (empty? queue) (= limit 1000))
      done
      (let [next-state (first queue)]
        (println next-state)
        (cond
          (is-finished? next-state)        (recur (rest queue) (conj done next-state) (inc limit))
          (is-hallway-empty? next-state)   (recur (apply conj (rest queue) (map #(apply-moves next-state %) (opening-moves next-state))) done (inc limit))
          (= 1 (hw-pod-count next-state))  (recur (conj (rest queue) (apply-moves next-state (in-process-moves next-state))) done (inc limit))
          (= 2 (hw-pod-count next-state))  (recur (conj (rest queue) (apply-moves next-state (two-hw-moves next-state))) done (inc limit))
          :else                            (do (println "wat?!") next-state))))))

(defn print-states-until-now
  [state]
  (let [states (->> state
                    (iterate reverse-move)
                    (take-while #(not-empty (% :moves)))
                    reverse)]
    (print-state (reverse-move (first states)))
    (println)
    (doseq [state states]
      (print-state state)
      (println)))) 

(let [state {[2 2] {:pod :C, :has-moved false}, [0 0] {:pod :empty}, [1 0] {:pod :empty}, :moves [[[4 1] [3 0]] [[6 1] [4 1]] [[6 2] [7 0]] [[3 0] [6 2]]], [4 2] {:pod :A, :has-moved false}, [3 0] {:pod :empty}, [9 0] {:pod :empty}, [8 0] {:pod :empty}, [4 1] {:pod :A, :has-moved true}, [8 2] {:pod :B, :has-moved false}, [10 0] {:pod :empty}, [8 1] {:pod :C, :has-moved false}, [6 1] {:pod :empty}, [7 0] {:pod :D, :has-moved true}, [2 0] {:pod :empty}, :cost 3072, [2 1] {:pod :D, :has-moved false}, [5 0] {:pod :empty}, [6 2] {:pod :B, :has-moved true}, [6 0] {:pod :empty}, [4 0] {:pod :empty}}]
  (from-to-moves state [8 2] 6)
  ;; (in-process-moves state)
  ;; (free-hallway-from-to-moves state [8 6])
  )

(doseq [state (run-simulation real-input)]
  (print-states-until-now state)
  (println (state :cost))
  )
