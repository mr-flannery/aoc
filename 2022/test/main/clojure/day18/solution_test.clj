(ns day18.solution-test
  (:require [clojure.test :refer :all]
            [day18.solution :refer :all]))

(deftest neighbors-test
  (testing "generates neighbors"
    (is
      (=
        (into #{} (neighbors [1 1 1]))
        (into #{} [[0 1 1] [2 1 1] [1 0 1] [1 2 1] [1 1 0] [1 1 2]]))))
  (testing "all neighbors are adjacent"
    (is
      (=
        (->> (neighbors [1 1 1]) (map #(are-adjacent? [1 1 1] %)) (reduce #(and %1 %2)))
        true))))
