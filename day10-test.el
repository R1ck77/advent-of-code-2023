(require 'day10)
(require 'buttercup)

(describe "Day 10"
  (describe "part 1"
    (it "replicates the example"
      (expect (day10/part-1 (advent/read-problem-lines 10 :example))
              :to-be 42))
    (xit "solves the problem"
      (expect (day10/part-1 (advent/read-problem-lines 10 :problem))
              :to-be 42)))
  (xdescribe "part 2"
    (it "replicates the example"
      (expect (day10/part-2 (advent/read-problem-lines 10 :example))
              :to-be 42))
    (xit "solves the problem"
      (expect (day10/part-2 (advent/read-problem-lines 10 :problem))
              :to-be 42))))
