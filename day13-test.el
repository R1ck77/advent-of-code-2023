(require 'day13)
(require 'buttercup)

(describe "--- Day 13: Point of Incidence ---"
  (describe "part 1"
    (it "replicates the example"
      (expect (day13/part-1 (advent/read-blocks-of-lines 13 :example))
              :to-be 405))
    (it "solves the problem"
      (expect (day13/part-1 (advent/read-blocks-of-lines 13 :problem))
              :to-be 36041)))
  (describe "part 2"
    (it "replicates the example"
      (expect (day13/part-2 (advent/read-blocks-of-lines 13 :example))
              :to-be 400))
    (it "solves the problem"
      (expect (day13/part-2 (advent/read-blocks-of-lines 13 :problem))
              :to-be 35915))))
