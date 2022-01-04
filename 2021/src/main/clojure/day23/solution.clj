(ns day23)

(def example-input ["#############"
                    "#...........#"
                    "###B#C#B#D###"
                    "  #A#D#C#A#  "
                    "  #########  "])

(def state {:hallway [nil nil nil nil nil nil nil nil nil nil nil]
            :1 [\B \A]
            :2 [\C \D]
            :3 [\B \C]
            :4 [\D \A]})

;; TODO: generate all possible steps at each state and then backtrack
