(ns day23-test
  (:require [clojure.test :refer [deftest testing is]]
            [day23 :as d23]))

(deftest possible-moves
  (testing "at the start"
    (let [state {[0 0]  {:pod :empty}
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
                 }]
      (testing "front room position"
        (is (=
             (sort [[0 0] [1 0] [3 0] [5 0] [7 0] [9 0] [10 0]])
             (sort (d23/possible-moves [2 1] :B state))))
        (is (=
             (sort [[0 0] [1 0] [3 0] [5 0] [7 0] [9 0] [10 0]])
             (sort (d23/possible-moves [4 1] :C state))))
        (is (=
             (sort [[0 0] [1 0] [3 0] [5 0] [7 0] [9 0] [10 0]])
             (sort (d23/possible-moves [6 1] :B state))))
        (is (=
             (sort [[0 0] [1 0] [3 0] [5 0] [7 0] [9 0] [10 0]])
             (sort (d23/possible-moves [8 1] :D state)))))
      (testing "back-room position"
        (is (empty? (d23/possible-moves [2 2] :A state)))
        (is (empty? (d23/possible-moves [4 2] :D state)))
        (is (empty? (d23/possible-moves [6 2] :C state)))
        (is (empty? (d23/possible-moves [8 2] :A state))))))
  (testing "in process"
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
      (is (=
           [[4 2]]
           (sort (d23/possible-moves [5 0] :A state))))
      (is (=
           [[0 0] [1 0] [4 2]]
           (sort (d23/possible-moves [3 0] :D state))))
      (is (=
           [[8 1] [9 0] [10 0]]
           (sort (d23/possible-moves [7 0] :B state)))))))

