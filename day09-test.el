(require 'day09)
(require 'buttercup)

(describe "--- Day 9: Mirage Maintenance ---"
  (describe "part 1"
    (it "replicates the example"
      (expect (day09/part-1 (advent/read-problem-lines 9 :example))
              :to-be 114))
    (it "solves the problem"
      (expect (day09/part-1 (advent/read-problem-lines 9 :problem))
              :to-be 1974232246)))
  (describe "part 2"
    (it "replicates the example"
      (expect (day09/part-2 (advent/read-problem-lines 9 :example))
              :to-be 2))
    (it "solves the problem"
      (expect (day09/part-2 (advent/read-problem-lines 9 :problem))
              :to-be 928))))
