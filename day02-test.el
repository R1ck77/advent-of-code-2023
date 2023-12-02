(require 'day02)
(require 'buttercup)

(describe "--- Day 2: Cube Conundrum ---"
  (describe "part 1"
    (it "replicates the example"
      (expect (day02/part-1 (advent/read-problem-lines 2 :example))
              :to-be 42))
    (xit "solves the problem"
      (expect (day02/part-1 (advent/read-problem-lines 2 :problem))
              :to-be 42)))
  (xdescribe "part 2"
    (it "replicates the example"
      (expect (day02/part-2 (advent/read-problem-lines 2 :example))
              :to-be 42))
    (xit "solves the problem"
      (expect (day02/part-2 (advent/read-problem-lines 2 :problem))
              :to-be 42))))
