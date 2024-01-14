(require 'day16)
(require 'buttercup)

(describe "Day 16"
  (describe "part 1"
    (it "replicates the example"
      (expect (day16/part-1 (advent/read-problem-lines 16 :example))
              :to-be 42))
    (xit "solves the problem"
      (expect (day16/part-1 (advent/read-problem-lines 16 :problem))
              :to-be 42)))
  (xdescribe "part 2"
    (it "replicates the example"
      (expect (day16/part-2 (advent/read-problem-lines 16 :example))
              :to-be 42))
    (xit "solves the problem"
      (expect (day16/part-2 (advent/read-problem-lines 16 :problem))
              :to-be 42))))
