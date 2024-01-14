(require 'day15)
(require 'buttercup)

(describe "--- Day 15: Lens Library ---"
  (describe "part 1"
    (it "replicates the example"
      (expect (day15/part-1 (advent/read-problem-lines 15 :example))
              :to-be 1320))
    (it "solves the problem"
      (expect (day15/part-1 (advent/read-problem-lines 15 :problem))
              :to-be 514639)))
  (describe "part 2"
    (it "replicates the example"
      (expect (day15/part-2 (advent/read-problem-lines 15 :example))
              :to-be 145))
    (it "solves the problem"
      (expect (day15/part-2 (advent/read-problem-lines 15 :problem))
              :to-be 279470))))
