(ns day19.solution-test
  (:require [clojure.test :refer :all]
            [day19.solution :refer :all]
            [matcher-combinators.test :refer :all]
            [matcher-combinators.matchers :refer :all]))

(def test-blueprint {:id 1 :o-o 4 :c-o 2 :ob-o 3 :ob-c 14 :g-o 2 :g-ob 7})

(deftest options-test
  (testing "only returns :geode-robot when one can be built"
    (is
      (=
        (options test-blueprint {:ore 2 :ore-robots 1 :clay 0 :clay-robots 0 :obsidian 7 :obsidian-robots 0 :geode 0 :geode-robots 0})
        '(:geode-robot)))
    (is
      (=
        (options test-blueprint {:ore 999999 :ore-robots 1 :clay 999999 :clay-robots 0 :obsidian 999999 :obsidian-robots 0 :geode 0 :geode-robots 0})
        '(:geode-robot))))
  (testing "if no geode robot con be built, always contains :none"
    (is (true?
          (.contains
            (options test-blueprint {:ore 0 :ore-robots 1 :clay 0 :clay-robots 0 :obsidian 0 :obsidian-robots 0 :geode 0 :geode-robots 0})
            :none)))
    (is (true?
          (.contains
            (options test-blueprint {:ore 1 :ore-robots 1 :clay 1 :clay-robots 0 :obsidian 1 :obsidian-robots 0 :geode 0 :geode-robots 0})
            :none))))
  (testing "obsidian > clay > ore > none"
    (is
      (=
        (options test-blueprint {:ore 4 :ore-robots 1 :clay 14 :clay-robots 0 :obsidian 0 :obsidian-robots 0 :geode 0 :geode-robots 0})
        '(:obsidian-robot :clay-robot :ore-robot :none)))
    (is
      (=
        (options test-blueprint {:ore 4 :ore-robots 1 :clay 13 :clay-robots 0 :obsidian 0 :obsidian-robots 0 :geode 0 :geode-robots 0})
        '(:clay-robot :ore-robot :none))))
  (testing "regressions"
    (is
      (=
        (options test-blueprint {:ore 2 :ore-robots 1 :clay 11 :clay-robots 4 :obsidian 2 :obsidian-robots 1 :geode 0 :geode-robots 0})
        '(:clay-robot :none)))))

(deftest build-robot-test
  (testing "builds a robot and correctly reduces the number of resources"
    (is (=
          (build-robot
            test-blueprint
            {:ore 4 :ore-robots 1 :clay 0 :clay-robots 0 :obsidian 7 :obsidian-robots 0 :geode 0 :geode-robots 0}
            :ore-robot)
          {:ore 0 :ore-robots 2 :clay 0 :clay-robots 0 :obsidian 7 :obsidian-robots 0 :geode 0 :geode-robots 0}))
    (is (=
          (build-robot
            test-blueprint
            {:ore 4 :ore-robots 1 :clay 0 :clay-robots 0 :obsidian 7 :obsidian-robots 0 :geode 0 :geode-robots 0}
            :clay-robot)
          {:ore 2 :ore-robots 1 :clay 0 :clay-robots 1 :obsidian 7 :obsidian-robots 0 :geode 0 :geode-robots 0}))
    (is (=
          (build-robot
            test-blueprint
            {:ore 4 :ore-robots 1 :clay 14 :clay-robots 0 :obsidian 7 :obsidian-robots 0 :geode 0 :geode-robots 0}
            :obsidian-robot)
          {:ore 1 :ore-robots 1 :clay 0 :clay-robots 0 :obsidian 7 :obsidian-robots 1 :geode 0 :geode-robots 0}))
    (is (=
          (build-robot
            test-blueprint
            {:ore 4 :ore-robots 1 :clay 14 :clay-robots 0 :obsidian 7 :obsidian-robots 0 :geode 0 :geode-robots 0}
            :geode-robot)
          {:ore 2 :ore-robots 1 :clay 14 :clay-robots 0 :obsidian 0 :obsidian-robots 0 :geode 0 :geode-robots 1}))))

(deftest next-states-test
  (testing "example from exercise"
    (is (=
          (next-states
            test-blueprint
            {:ore 0 :ore-robots 1 :clay 0 :clay-robots 0 :obsidian 0 :obsidian-robots 0 :geode 0 :geode-robots 0}
            0)
          '([{:ore 1 :ore-robots 1 :clay 0 :clay-robots 0 :obsidian 0 :obsidian-robots 0 :geode 0 :geode-robots 0} 1])))
    (is (=
          (next-states
            test-blueprint
            {:ore 1 :ore-robots 1 :clay 0 :clay-robots 0 :obsidian 0 :obsidian-robots 0 :geode 0 :geode-robots 0}
            1)
          '([{:ore 2 :ore-robots 1 :clay 0 :clay-robots 0 :obsidian 0 :obsidian-robots 0 :geode 0 :geode-robots 0} 2])))
    (is (match?
          (in-any-order
            [[{:ore 1 :ore-robots 1 :clay 0 :clay-robots 1 :obsidian 0 :obsidian-robots 0 :geode 0 :geode-robots 0} 3]
             [{:ore 3 :ore-robots 1 :clay 0 :clay-robots 0 :obsidian 0 :obsidian-robots 0 :geode 0 :geode-robots 0} 3]])
          (next-states
            test-blueprint
            {:ore 2 :ore-robots 1 :clay 0 :clay-robots 0 :obsidian 0 :obsidian-robots 0 :geode 0 :geode-robots 0}
            2)))
    (is (=
          (next-states
            test-blueprint
            {:ore 1 :ore-robots 1 :clay 0 :clay-robots 1 :obsidian 0 :obsidian-robots 0 :geode 0 :geode-robots 0}
            3)
          '([{:ore 2 :ore-robots 1 :clay 1 :clay-robots 1 :obsidian 0 :obsidian-robots 0 :geode 0 :geode-robots 0} 4])))
    (is (match?
          (in-any-order
            [[{:ore 3 :ore-robots 1 :clay 2 :clay-robots 1 :obsidian 0 :obsidian-robots 0 :geode 0 :geode-robots 0} 5]
             [{:ore 1 :ore-robots 1 :clay 2 :clay-robots 2 :obsidian 0 :obsidian-robots 0 :geode 0 :geode-robots 0} 5]])
          (next-states
            test-blueprint
            {:ore 2 :ore-robots 1 :clay 1 :clay-robots 1 :obsidian 0 :obsidian-robots 0 :geode 0 :geode-robots 0}
            4)))
    (is (match?
          (in-any-order
            [[{:ore 2 :ore-robots 1 :clay 4 :clay-robots 2 :obsidian 0 :obsidian-robots 0 :geode 0 :geode-robots 0} 6]])
          (next-states
            test-blueprint
            {:ore 1 :ore-robots 1 :clay 2 :clay-robots 2 :obsidian 0 :obsidian-robots 0 :geode 0 :geode-robots 0}
            5)))
    (is (match?
          (in-any-order
            [[{:ore 3 :ore-robots 1 :clay 6 :clay-robots 2 :obsidian 0 :obsidian-robots 0 :geode 0 :geode-robots 0} 7]
             [{:ore 1 :ore-robots 1 :clay 6 :clay-robots 3 :obsidian 0 :obsidian-robots 0 :geode 0 :geode-robots 0} 7]])
          (next-states
            test-blueprint
            {:ore 2 :ore-robots 1 :clay 4 :clay-robots 2 :obsidian 0 :obsidian-robots 0 :geode 0 :geode-robots 0}
            6)))
    (is (match?
          (in-any-order
            [[{:ore 2 :ore-robots 1 :clay 9 :clay-robots 3 :obsidian 0 :obsidian-robots 0 :geode 0 :geode-robots 0} 8]])
          (next-states
            test-blueprint
            {:ore 1 :ore-robots 1 :clay 6 :clay-robots 3 :obsidian 0 :obsidian-robots 0 :geode 0 :geode-robots 0}
            7)))
    (is (match?
          (in-any-order
            [[{:ore 3 :ore-robots 1 :clay 12 :clay-robots 3 :obsidian 0 :obsidian-robots 0 :geode 0 :geode-robots 0} 9]
             [{:ore 1 :ore-robots 1 :clay 12 :clay-robots 4 :obsidian 0 :obsidian-robots 0 :geode 0 :geode-robots 0} 9]])
          (next-states
            test-blueprint
            {:ore 2 :ore-robots 1 :clay 9 :clay-robots 3 :obsidian 0 :obsidian-robots 0 :geode 0 :geode-robots 0} 8)))
    (is (match?
          (in-any-order
            [[{:ore 4 :ore-robots 1 :clay 15 :clay-robots 3 :obsidian 0 :obsidian-robots 0 :geode 0 :geode-robots 0} 10]
             [{:ore 2 :ore-robots 1 :clay 15 :clay-robots 4 :obsidian 0 :obsidian-robots 0 :geode 0 :geode-robots 0} 10]])
          (next-states
            test-blueprint
            {:ore 3 :ore-robots 1 :clay 12 :clay-robots 3 :obsidian 0 :obsidian-robots 0 :geode 0 :geode-robots 0} 9)))
    (is (match?
          (in-any-order
            [[{:ore 5 :ore-robots 1 :clay 18 :clay-robots 3 :obsidian 0 :obsidian-robots 0 :geode 0 :geode-robots 0} 11]
             [{:ore 3 :ore-robots 1 :clay 18 :clay-robots 4 :obsidian 0 :obsidian-robots 0 :geode 0 :geode-robots 0} 11]
             [{:ore 1 :ore-robots 2 :clay 18 :clay-robots 3 :obsidian 0 :obsidian-robots 0 :geode 0 :geode-robots 0} 11]
             [{:ore 2 :ore-robots 1 :clay 4 :clay-robots 3 :obsidian 0 :obsidian-robots 1 :geode 0 :geode-robots 0} 11]])
          (next-states
            test-blueprint
            {:ore 4 :ore-robots 1 :clay 15 :clay-robots 3 :obsidian 0 :obsidian-robots 0 :geode 0 :geode-robots 0} 10)))
    (is (match?
          (in-any-order
            [[{:ore 3 :ore-robots 1 :clay 7 :clay-robots 3 :obsidian 1 :obsidian-robots 1 :geode 0 :geode-robots 0} 12]
             [{:ore 1 :ore-robots 1 :clay 7 :clay-robots 4 :obsidian 1 :obsidian-robots 1 :geode 0 :geode-robots 0} 12]])
          (next-states
            test-blueprint
            {:ore 2 :ore-robots 1 :clay 4 :clay-robots 3 :obsidian 0 :obsidian-robots 1 :geode 0 :geode-robots 0} 11)))
    (is (match?
          (in-any-order
            [[{:ore 2 :ore-robots 1 :clay 11 :clay-robots 4 :obsidian 2 :obsidian-robots 1 :geode 0 :geode-robots 0} 13]])
          (next-states
            test-blueprint
            {:ore 1 :ore-robots 1 :clay 7 :clay-robots 4 :obsidian 1 :obsidian-robots 1 :geode 0 :geode-robots 0} 12)))
    (is (match?
          (in-any-order
            [[{:ore 3 :ore-robots 1 :clay 15 :clay-robots 4 :obsidian 3 :obsidian-robots 1 :geode 0 :geode-robots 0} 14]
             [{:ore 1 :ore-robots 1 :clay 15 :clay-robots 5 :obsidian 3 :obsidian-robots 1 :geode 0 :geode-robots 0} 14]])
          (next-states
            test-blueprint
            {:ore 2 :ore-robots 1 :clay 11 :clay-robots 4 :obsidian 2 :obsidian-robots 1 :geode 0 :geode-robots 0} 13)))))
