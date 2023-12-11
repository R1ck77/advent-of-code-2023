(require 'day08)
(require 'buttercup)

(describe "--- Day 8: Haunted Wasteland ---"
  (describe "part 1"
    (it "replicates the example"
      (expect (day08/part-1 (advent/read-problem-lines 8 :example))
              :to-be 6))
    (it "solves the problem"
      (expect (day08/part-1 (advent/read-problem-lines 8 :problem))
              :to-be 22411)))
  (describe "part 2"
    (it "replicates the example"
      (expect (day08/part-2 (advent/read-problem-lines 8 :example 2))
              :to-be 6))
    (it "solves the problem"
      (expect (day08/part-2 (advent/read-problem-lines 8 :problem))
              :to-be 11188774513823))))
