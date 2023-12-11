(require 'day07)
(require 'buttercup)

(describe "--- Day 7: Camel Cards ---"
  (describe "part 1"
    (it "replicates the example"
      (expect (day07/part-1 (advent/read-problem-lines 7 :example))
              :to-be 6440))
    (it "solves the problem"
      (expect (day07/part-1 (advent/read-problem-lines 7 :problem))
              :to-be 248559379)))
  (xdescribe "part 2"
    (it "replicates the example"
      (expect (day07/part-2 (advent/read-problem-lines 7 :example))
              :to-be 42))
    (xit "solves the problem"
      (expect (day07/part-2 (advent/read-problem-lines 7 :problem))
              :to-be 42))))
