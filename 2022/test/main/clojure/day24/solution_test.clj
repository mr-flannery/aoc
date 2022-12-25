(ns day24.solution-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [day24.solution :refer :all]
            [clojure.string :as str]))

(def test-field
  {[0 0] []
   [0 1] []
   [0 2] []
   [1 0] []
   [1 1] []
   [1 2] []
   [2 0] []
   [2 1] []
   [2 2] []})

(deftest possible-moves-test
  (testing "returns 5 moves on an empty field not next to a wall"
    (is
      (=
        (possible-moves test-field [1 1] 2 2)
        [[2 1] [1 2] [1 1] [0 1] [1 0]])))
  (testing "returns 4 moves on an empty field next to one wall"
    (is
      (=
        (possible-moves test-field [0 1] 2 2)
        [[1 1] [0 2] [0 1] [0 0]])))
  (testing "returns 3 moves on an empty field next to two walls"
    (is
      (=
        (possible-moves test-field [0 2] 2 2)
        [[1 2] [0 2] [0 1]])))
  (testing "can't wait if there's a blizzard on the player pos"
    (is
      (=
        (possible-moves (assoc test-field [1 1] [\>]) [1 1] 3 3)
        [[2 1] [1 2] [0 1] [1 0]])))
  (testing "can't move to a field with a blizzard"
    (is
      (=
        (possible-moves
          (-> test-field
              (assoc [1 1] [\>])
              (assoc [2 1] [\>]))
          [1 1] 2 2)
        [[1 2] [0 1] [1 0]]))
    (is
      (=
        (possible-moves
          (-> test-field
              (assoc [1 1] [\>])
              (assoc [2 1] [\>])
              (assoc [1 2] [\>]))
          [1 1] 2 2)
        [[0 1] [1 0]]))
    (is
      (=
        (possible-moves
          (-> test-field
              (assoc [1 1] [\>])
              (assoc [2 1] [\>])
              (assoc [1 2] [\>])
              (assoc [0 1] [\>]))
          [1 1] 2 2)
        [[1 0]]))
    (is
      (=
        (possible-moves
          (-> test-field
              (assoc [1 1] [\>])
              (assoc [2 1] [\>])
              (assoc [1 2] [\>])
              (assoc [0 1] [\>])
              (assoc [1 0] [\>]))
          [1 1] 2 2)
        [])))
  (testing "I can wait at the starting pos"
    (is (true? (.contains (possible-moves test-field [-1 0] 2 2) [-1 0]))))
  (testing "real input: turn 0"
    (is
      (=
        (possible-moves field [-1 0] 34 99)
        [[-1 0]])))
  (testing "real input: turn 1"
    (is
      (=
        (possible-moves (-> field (play-round 34 99)) [-1 0] 34 99)
        [[-1 0]])))
  (testing "real input: turn 2"
    (is
      (=
        (possible-moves (-> field (play-round 34 99) (play-round 34 99)) [-1 0] 34 99)
        [[-1 0]])))
  (testing "real input: turn 3"
    (is
      (=
        (possible-moves (-> field (play-round 34 99) (play-round 34 99) (play-round 34 99)) [-1 0] 34 99)
        [[0 0] [-1 0]])))
  (def sample-field-2 (play-n-rounds sample-field 18 3 5))
  
  (testing "sample input part2 turn 18"
    (is
      (=
        (possible-moves sample-field-2 [4 5] 3 5)
        [[4 5]])))
  (testing "sample input part2 turn 19"
    (is
      (=
        (possible-moves (-> sample-field-2
                            (play-round 3 5)) [4 5] 3 5)
        [[4 5]])))
  (testing "sample input part2 turn 19"
    (is
      (=
        (possible-moves (-> sample-field-2
                            (play-round 3 5)
                            (play-round 3 5)) [4 5] 3 5)
        [[4 5] [3 5]]))))
