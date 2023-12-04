(require 'day04)
(require 'buttercup)

(describe "--- Day 4: Scratchcards ---"
  (describe "part 1"
    (it "replicates the example"
      (expect (day04/part-1 (advent/read-problem-lines 4 :example))
              :to-be 13))
    (it "solves the problem"
      (expect (day04/part-1 (advent/read-problem-lines 4 :problem))
              :to-be 28750)))
  (describe "part 2"
    (it "replicates the example"
      (expect (day04/part-2 (advent/read-problem-lines 4 :example))
              :to-be 30))
    (it "solves the problem"
      (expect (day04/part-2 (advent/read-problem-lines 4 :problem))
              :to-be 10212704))))
