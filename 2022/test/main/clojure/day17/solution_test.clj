(ns day17.solution-test
  (:require [clojure.test :refer :all]
            [day17.solution :refer :all]))

(deftest apply-jet-test
  (testing "jet left"
    (is
      (=
        #{[0 0] [1 0] [2 0] [3 0]}
        (apply-jet \< '() #{[0 0] [1 0] [2 0] [3 0]}))
      "cannot jet left when at the left most position")
    (is
      (=
        #{[0 0] [1 0] [2 0] [3 0]}
        (apply-jet \< '() #{[1 0] [2 0] [3 0] [4 0]})))
    (is
      (=
        #{[1 0] [2 0] [3 0] [4 0]}
        (apply-jet \< '(#{[0 0] [0 1] [0 2] [0 3]}) #{[1 0] [2 0] [3 0] [4 0]})))
    (is
      (=
        #{[1 0] [2 0] [3 0] [4 0]}
        (apply-jet \< '(#{[0 0] [0 1] [0 2] [0 3]}) #{[2 0] [3 0] [4 0] [5 0]}))))
  (testing "jet right"
    (is
      (=
        #{[1 0] [2 0] [3 0] [4 0]}
        (apply-jet \> '() #{[0 0] [1 0] [2 0] [3 0]})))
    (is
      (=
        #{[3 0] [4 0] [5 0] [6 0]}
        (apply-jet \> '() #{[3 0] [4 0] [5 0] [6 0]})))
    (is
      (= #{[2 0] [3 0] [4 0] [5 0]}
         (apply-jet \> '(#{[6 0] [6 1] [6 2] [6 3]}) #{[2 0] [3 0] [4 0] [5 0]})))
    (is
      (=
        #{[2 0] [3 0] [4 0] [5 0]}
        (apply-jet \> '(#{[6 0] [6 1] [6 2] [6 3]}) #{[1 0] [2 0] [3 0] [4 0]})))))

(deftest can-fall-test
  (testing "cannot fall if at the bottom of the pit"
    (is
      (false? (can-fall? '() #{[0 0] [1 0] [2 0] [3 0]})))
    (is
      (false? (can-fall? '() #{[0 0] [1 0] [0 1] [1 1]}))))
  (testing "cannot fall if it would collide with the pile"
    (is
      (false? (can-fall? '(#{[0 0] [1 0] [0 1] [1 1]}) #{[0 1] [1 1] [0 2] [1 3]}))))
  (testing "can fall if it would not collide with the pile"
    (is
      (true? (can-fall? '(#{[0 0] [1 0] [2 0] [3 0]}) #{[0 2] [1 2] [3 2] [4 2]})))))

(deftest next-piece-test
  (testing "next piece's left most x index is always 2"
    (is
      (= (->> (plus 0) (map first) (apply min)) 2))
    (is
      (= (->> (horizontal-line 0) (map first) (apply min)) 2))
    (is
      (= (->> (vertical-line 0) (map first) (apply min)) 2))
    (is
      (= (->> (block 0) (map first) (apply min)) 2))
    (is
      (= (->> (reverse-l 0) (map first) (apply min)) 2)))
  ;(testing "next piece's bottom y is always 4 higher than the top of the pile"
  ;  (is
  ;    (=
  ;      (->> (next-piece horizontal-line '()) (map second) (apply min))
  ;      3))
  ;  (is
  ;    (=
  ;      (->> (next-piece horizontal-line '(#{[0 0] [1 0] [2 0] [3 0]})) (map second) (apply min))
  ;      4))
  ;  (is
  ;    (=
  ;      (->> (next-piece horizontal-line '(#{[0 0] [1 0] [2 0] [2 1] [2 2]})) (map second) (apply min))
  ;      6))
  ;  (is
  ;    (=
  ;      (->> (next-piece horizontal-line '(#{[0 1] [1 1] [0 2] [2 2]} #{[0 0] [1 0] [2 0] [2 1] [2 2]})) (map second) (apply min))
  ;      6))
  ;  (is
  ;    (=
  ;      (->> (next-piece plus '(#{[2 0] [3 0] [4 0] [5 0]})) (map second) (apply min))
  ;      4))
  ;  (is
  ;    (=
  ;      (->> (next-piece reverse-l '(#{[1 2] [2 1] [2 2] [2 3] [3 2]} #{[2 0] [3 0] [4 0] [5 0]})) (map second) (apply min))
  ;      7)))
  )

(deftest are-halves-equivalent?-test
  (is
    (true?
      (time (are-halves-equivalent? (atom [#{[3 0] [2 0] [5 0] [4 0]}
                                     #{[2 2] [3 3] [4 2] [3 1] [3 2]}
                                     #{[2 3] [2 5] [1 3] [0 3] [2 4]}
                                     #{[4 3] [4 6] [4 5] [4 4]}
                                     #{[4 7] [5 7] [4 8] [5 8]}
                                     #{[3 9] [2 9] [5 9] [4 9]}
                                     #{[2 11] [3 12] [4 11] [3 10] [3 11]}
                                     #{[2 12] [2 14] [1 12] [0 12] [2 13]}
                                     #{[4 12] [4 15] [4 14] [4 13]}
                                     #{[4 16] [5 16] [4 17] [5 17]}]) (atom 10)))))
  (is
    (true?
      (time (are-halves-equivalent?2 (atom [#{[3 0] [2 0] [5 0] [4 0]}
                                           #{[2 2] [3 3] [4 2] [3 1] [3 2]}
                                           #{[2 3] [2 5] [1 3] [0 3] [2 4]}
                                           #{[4 3] [4 6] [4 5] [4 4]}
                                           #{[4 7] [5 7] [4 8] [5 8]}
                                           #{[3 9] [2 9] [5 9] [4 9]}
                                           #{[2 11] [3 12] [4 11] [3 10] [3 11]}
                                           #{[2 12] [2 14] [1 12] [0 12] [2 13]}
                                           #{[4 12] [4 15] [4 14] [4 13]}
                                           #{[4 16] [5 16] [4 17] [5 17]}]) (atom 10))))))
