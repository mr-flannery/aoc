(ns day22.solution-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [day22.solution :refer :all]
            [clojure.string :as str]))

(def test-input (slurp (io/resource "day22/test-input.txt")))

(def test-field (let [[field insts] (parse test-input)]
                  field))

(def max-l (->> test-field keys (map first) (apply max) inc) )
(def max-c (->> test-field keys (map second) (apply max) inc) )

(deftest next-pos-test
  (testing "gives the next-pos"
    (is 
      (= 
        (next-pos [9 3] :south max-l max-c)
        [0 3]))))

(deftest move-test
  (testing "moves are made correctly"
    (is
      (=
        (move test-field [0 0] 10 :east max-l max-c)
        [0 3]))
    (is
      (=
        (move test-field [0 3] 3 :south max-l max-c)
        [3 3]))
    (is
      (=
        (move test-field [3 3] 5 :west max-l max-c)
        [3 3]))
    (is
      (=
        (move test-field [3 3] 7 :south max-l max-c)
        [0 3]))))

(deftest move2-test
  (testing "from A"
    (is (=
          (move2 [150 0] :north 10 (first field-and-insts) 200 150)
          [[148 0] :north]))
    (is (=
          (move2 [156 49] :east 10 (first field-and-insts) 200 150)
          [[140 56] :north]))
    (is (=
          (move2 [199 25] :south 10 (first field-and-insts) 200 150)
          [[2 125] :south]))
    (is (=
          (move2 [199 23] :south 10 (first field-and-insts) 200 150)
          [[199 23] :south]))
    (is (=
          (move2 [174 0] :west 10 (first field-and-insts) 200 150)
          [[7 74] :south])))
  (testing "from B"
    (is (=
          (move2 [100 49] :north 10 (first field-and-insts) 200 150)
          [[99 53] :east]))
    (is (=
          (move2 [101 49] :east 10 (first field-and-insts) 200 150)
          [[101 58] :east]))
    (is (=
          (move2 [149 49] :south 10 (first field-and-insts) 200 150)
          [[153 49] :south]))
    (is (=
          (move2 [149 47] :south 10 (first field-and-insts) 200 150)
          [[149 47] :south]))
    (is (=
          (move2 [100 0] :west 10 (first field-and-insts) 200 150)
          [[49 56] :east])))
  (testing "from C"
    (is (=
          (move2 [100 50] :north 10 (first field-and-insts) 200 150)
          [[97 50] :north]))
    (is (=
          (move2 [100 99] :east 10 (first field-and-insts) 200 150)
          [[49 145] :west]))
    (is (=
          (move2 [149 98] :south 10 (first field-and-insts) 200 150)
          [[198 46] :west]))
    (is (=
          (move2 [149 50] :west 10 (first field-and-insts) 200 150)
          [[149 40] :west])))
  (testing "from D"
    (is (=
          (move2 [50 50] :north 10 (first field-and-insts) 200 150)
          [[45 50] :north]))
    (is (=
          (move2 [56 99] :east 10 (first field-and-insts) 200 150)
          [[49 106] :north]))
    (is (=
          (move2 [99 99] :south 10 (first field-and-insts) 200 150)
          [[107 99] :south]))
    (is (=
          (move2 [99 50] :west 10 (first field-and-insts) 200 150)
          [[101 49] :south]))
    (is (=
          (move2 [98 50] :west 10 (first field-and-insts) 200 150)
          [[98 50] :west])))
  (testing "from E"
    (is (=
          (move2 [0 50] :north 10 (first field-and-insts) 200 150)
          [[150 9] :east]))
    (is (=
          (move2 [49 99] :east 10 (first field-and-insts) 200 150)
          [[49 109] :east]))
    (is (=
          (move2 [49 99] :south 10 (first field-and-insts) 200 150)
          [[57 99] :south]))
    (is (=
          (move2 [49 50] :west 10 (first field-and-insts) 200 150)
          [[100 9] :east])))
  (testing "from F"
    (is (=
          (move2 [0 149] :north 10 (first field-and-insts) 200 150)
          [[190 49] :north]))
    (is (=
          (move2 [49 149] :east 10 (first field-and-insts) 200 150)
          [[100 90] :west]))
    (is (=
          (move2 [49 101] :south 10 (first field-and-insts) 200 150)
          [[51 97] :west]))
    (is (=
          (move2 [49 100] :west 10 (first field-and-insts) 200 150)
          [[49 90] :west]))))
