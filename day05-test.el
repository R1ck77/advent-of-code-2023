(require 'day05)
(require 'buttercup)

(describe "--- Day 5: If You Give A Seed A Fertilizer ---"
  (describe "part 1"
    (it "replicates the example"
      (expect (day05/part-1 (advent/read-blocks-of-lines 5 :example))
              :to-be 35))
    (it "solves the problem"
      (expect (day05/part-1 (advent/read-blocks-of-lines 5 :problem))
              :to-be 26273516)))
  (describe "part 2"
    (it "replicates the example"
      (expect (day05/part-2 (advent/read-blocks-of-lines 5 :example))
              :to-be 46))
    (it "solves the problem"
      (expect (day05/part-2 (advent/read-blocks-of-lines 5 :problem))
              :to-be 34039469))))
