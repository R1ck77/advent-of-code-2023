(require 'day16)
(require 'buttercup)

(describe "--- Day 16: The Floor Will Be Lava ---"
  (describe "part 1"
    (it "replicates the example"
      (expect (day16/part-1 (advent/read-problem-lines 16 :example))
              :to-be 46))
    (it "solves the problem"
      (expect (day16/part-1 (advent/read-problem-lines 16 :problem))
              :to-be 7543)))
  (describe "part 2"
    (it "replicates the example"
      (expect (day16/part-2 (advent/read-problem-lines 16 :example))
              :to-be 51))
    (it "solves the problem"
      (expect (day16/part-2 (advent/read-problem-lines 16 :problem))
              :to-be 8231))))
