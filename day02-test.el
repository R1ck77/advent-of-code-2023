(require 'day02)
(require 'buttercup)

(describe "--- Day 2: Cube Conundrum ---"
  (describe "part 1"
    (it "replicates the example"
      (expect (day02/part-1 (advent/read-problem-lines 2 :example))
              :to-be 8))
    (it "solves the problem"
      (expect (day02/part-1 (advent/read-problem-lines 2 :problem))
              :to-be 2169)))
  (describe "part 2"
    (it "replicates the example"
      (expect (day02/part-2 (advent/read-problem-lines 2 :example))
              :to-be 2286))
    (it "solves the problem"
      (expect (day02/part-2 (advent/read-problem-lines 2 :problem))
              :to-be 60948))))
