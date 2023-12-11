(require 'day06)
(require 'buttercup)

(describe "--- Day 6: Wait For It ---"
  (describe "part 1"
    (it "replicates the example"
      (expect (day06/part-1 (advent/read-problem-lines 6 :example))
              :to-be 288))
    (it "solves the problem"
      (expect (day06/part-1 (advent/read-problem-lines 6 :problem))
              :to-be 1710720)))
  (describe "part 2"
    (it "replicates the example"
      (expect (day06/part-2 (advent/read-problem-lines 6 :example))
              :to-be 71503))
    (it "solves the problem"
      (expect (day06/part-2 (advent/read-problem-lines 6 :problem))
              :to-be 35349468))))
