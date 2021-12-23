(ns day18)

;; addition
(defn add
  [a b]
  [a b])

;; detect tree depths
(defn depth
  ([node]
   (depth node 0))
  ([node level]
   (if (number? node)
     level
     (let [[left right] node]
       (max level (depth left (inc level)) (depth right (inc level)))))))

;; (let [a [[1 2] [[3 4] 5]]
;;       b [[[[[9,8],1],2],3],4]
;;       c [7,[6,[5,[4,[3,2]]]]]]
;;   (depth c))

;; explosion
(defn complete-left
  [tree path]
  (loop [path (into [] path)]
    (if (number? (get-in tree path))
      path
      (let [[left _] (get-in tree path)]
        (if (number? left)
          (conj path 0)
          (recur (conj path 0)))))))

(defn complete-right
  [tree path]
  (loop [path (into [] path)]
    (if (number? (get-in tree path))
      path
      (let [[_ right] (get-in tree path)]
        (if (number? right)
          (conj path 1)
          (recur (conj path 1)))))))

(defn update-left-path
  [tree target-path]
  (->> target-path
       reverse
       (drop-while #(= % 0))
       rest
       reverse
       (into [])
       (#(conj % 0))
       (complete-right tree)))

(defn update-right-path
  [tree target-path]
  (->> target-path
       reverse
       (drop-while #(= % 1))
       rest
       reverse
       (into [])
       (#(conj % 1))
       (complete-left tree)))

(defn update-left
  [tree target-path value]
  (if (every? zero? target-path)
    tree
    (update-in tree (update-left-path tree target-path) #(+ % value))))

(defn update-right
  [tree target-path value]
  (if (every? #(= % 1) target-path)
    tree
    (update-in tree (update-right-path tree target-path) #(+ % value))))

(defn explode
  ;; {:test (fn []
  ;;          (is (=
  ;;               (explode [[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]] [0 1 1 1])
  ;;               [[3 [2 [8 0]]] [9 [5 [4 [3 2]]]]])))}
  [tree path]
  (let [[left right] (get-in tree path)
        update-left-path (update-left-path tree path)
        update-right-path (update-right-path tree path)]
    (-> tree
        (assoc-in path 0)
        (update-left path left)
        (update-right path right))))

;; (explode [[[[0 [4 5]] [0 0]] [[[4 5] [2 6]] [9 5]]] [7 [[[3 7] [4 3]] [[6 3] [8 8]]]]]
;;          [0 1 0 0])

;; (explode [[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]] [0 1 1 1])
;; (explode [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]] [1 1 1 1])
;; (explode [7,[6,[5,[4,[3,2]]]]] [1 1 1 1])
;; (explode [[[[[9,8],1],2],3],4] [0 0 0 0])

;; splitting
(defn split
  [tree path]
  (let [target      (get-in tree path)
        new-left    (int (Math/floor (/ target 2)))
        new-right   (int (Math/ceil (/ target 2)))]
    (assoc-in tree path [new-left new-right])))

;; (split [[[[0,7],4],[15,[0,13]]],[1,1]] [0 1 0])

;; das isn bisschen sehr umständlich...
;; für explode kann ich stattdessen einfach links runtergehen
;; sobald mein Pfad die länge 4 hat, nehme ich den linksten Knoten, für den left und right numbers sind
(defn all-leaf-paths
  [tree]
  (loop [stack  [[1] [0]]
         result []]
    (if (empty? stack)
      result
      (let [next         (peek stack)
            rest         (pop stack)]
        (if (number? (get-in tree next)) ;; in case one of the root's children is a leaf
          (recur rest (conj result next))
          (let [[left right] (get-in tree next)
                next-stack   (-> rest
                                 (#(if (number? left) (conj %) (conj % (conj next 0))))
                                 (#(if (number? right) (conj %) (conj % (conj next 1)))))
                next-result  (-> result
                                 (#(if (number? left) (conj % (conj next 0)) (conj %)))
                                 (#(if (number? right) (conj % (conj next 1)) (conj %))))]
            (recur next-stack next-result)))))))

(defn find-next-exploder
  [tree]
  (loop [stack [[]]]
    (if (empty? stack)
      nil
      (let [next         (peek stack)
            rest         (pop stack)
            [left right] (get-in tree next)]
        (if (and (number? left) (number? right) (>= (count next) 4))
          next ;; <= result
          (let [next-stack (-> rest
                               (#(if (number? right) (conj %) (conj % (conj next 1))))
                               (#(if (number? left) (conj %) (conj % (conj next 0)))))]
            (recur next-stack)))))))



;; (find-next-exploder (reduce add [[1,1] [2,2] [3,3] [4,4]]))
;; (find-next-exploder (reduce add [[1,1] [2,2] [3,3] [4,4] [5 5]]))
;; (find-next-exploder [[[[[9,8],1],2],3],4])

(defn find-next-splitter
  [tree]
  (loop [stack [[]]]
    (if (empty? stack)
      nil
      (let [next         (peek stack)
            rest         (pop stack)
            [left right] (get-in tree next)]
        (if (and (number? left) (>= left 10))
          (conj next 0)
          (if (and (number? right) (>= right 10))
            (conj next 1)
            (let [next-stack (-> rest
                                 (#(if (number? right) (conj %) (conj % (conj next 1))))
                                 (#(if (number? left) (conj %) (conj % (conj next 0)))))]
              (recur next-stack))))))))

;; detect if and where an action needs to take place
(defn reduce-sf-number
  [tree]
  (loop [tree tree]
    (let [next-exploder (find-next-exploder tree)
          next-splitter (find-next-splitter tree)]
      (println tree)
      (println next-exploder)
      (println next-splitter)
      (println)
      (if (some? next-exploder)
        (recur (explode tree next-exploder))
        (if (some? next-splitter)
          (recur (split tree next-splitter))
          tree)))))

;; wtf
;; wasn hier der bug bitter?
(defn step
  [sfn1 sfn2]
  (reduce-sf-number (add sfn1 sfn2)))

(reduce step [[1,1] [2,2] [3,3] [4,4]])
(reduce step [[1,1] [2,2] [3,3] [4,4] [5 5]])
(reduce step [[1,1] [2,2] [3,3] [4,4] [5 5] [6 6]])

(step [[[[4,3],4],4],[7,[[8,4],9]]] [1,1])

(reduce step [[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
              [7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
              [[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
              [[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
              ])

(reduce step [[[[[7 0] [7 7]] [[7 7] [7 8]]] [[[7 7] [8 8]] [[7 7] [8 7]]]]
              [7,[5,[[3,8],[1,4]]]]])

;; fail
(reduce step [[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
              [7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
              [[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
              [[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
              [7,[5,[[3,8],[1,4]]]]
              [[2,[2,2]],[8,[8,1]]]
              [2,9]
              [1,[[[9,3],9],[[9,0],[0,7]]]]
              [[[5,[7,4]],7],1]
              [[[[4,2],2],6],[8,7]]])

(reduce step [[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
              [[[5,[2,8]],4],[5,[[9,9],0]]]
              [6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
              [[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
              [[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
              [[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
              [[[[5,4],[7,7]],8],[[8,3],8]]
              [[9,3],[[9,9],[6,[4,9]]]]
              [[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
              [[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]])