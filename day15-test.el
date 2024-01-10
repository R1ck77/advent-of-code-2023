(require 'day15)
(require 'buttercup)

(describe "Day 15"
  (describe "part 1"
    (it "replicates the example"
      (expect (day15/part-1 (advent/read-problem-lines 15 :example))
              :to-be 42))
    (xit "solves the problem"
      (expect (day15/part-1 (advent/read-problem-lines 15 :problem))
              :to-be 42)))
  (xdescribe "part 2"
    (it "replicates the example"
      (expect (day15/part-2 (advent/read-problem-lines 15 :example))
              :to-be 42))
    (xit "solves the problem"
      (expect (day15/part-2 (advent/read-problem-lines 15 :problem))
              :to-be 42))))
