(require 'day01)
(require 'buttercup)

(describe "--- Day 1: Trebuchet?! ---"
  (describe "part 1"
    (it "replicates the example"
      (expect (day01/part-1 (advent/read-problem-lines 1 :example))
              :to-be 142))
    (it "solves the problem"
      (expect (day01/part-1 (advent/read-problem-lines 1 :problem))
              :to-be 54953)))
  (describe "part 2"
    (it "replicates the example"
      (expect (day01/part-2 (advent/read-problem-lines 1 :example 2))
              :to-be 281))
    (xit "solves the problem"
      (expect (day01/part-2 (advent/read-problem-lines 1 :problem))
              :to-be 42))))
