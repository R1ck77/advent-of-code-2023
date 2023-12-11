(require 'day12)
(require 'buttercup)

(describe "Day 12"
  (describe "part 1"
    (it "replicates the example"
      (expect (day12/part-1 (advent/read-problem-lines 12 :example))
              :to-be 42))
    (xit "solves the problem"
      (expect (day12/part-1 (advent/read-problem-lines 12 :problem))
              :to-be 42)))
  (xdescribe "part 2"
    (it "replicates the example"
      (expect (day12/part-2 (advent/read-problem-lines 12 :example))
              :to-be 42))
    (xit "solves the problem"
      (expect (day12/part-2 (advent/read-problem-lines 12 :problem))
              :to-be 42))))
