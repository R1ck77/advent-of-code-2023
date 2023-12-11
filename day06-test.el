(require 'day06)
(require 'buttercup)

(describe "Day 6"
  (describe "part 1"
    (it "replicates the example"
      (expect (day06/part-1 (advent/read-problem-lines 6 :example))
              :to-be 42))
    (xit "solves the problem"
      (expect (day06/part-1 (advent/read-problem-lines 6 :problem))
              :to-be 42)))
  (xdescribe "part 2"
    (it "replicates the example"
      (expect (day06/part-2 (advent/read-problem-lines 6 :example))
              :to-be 42))
    (xit "solves the problem"
      (expect (day06/part-2 (advent/read-problem-lines 6 :problem))
              :to-be 42))))
