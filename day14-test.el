(require 'day14)
(require 'buttercup)

(describe "--- Day 14: Parabolic Reflector Dish ---"
  (describe "part 1"
    (it "replicates the example"
      (expect (day14/part-1 (advent/read-problem-lines 14 :example))
              :to-be 136))
    (it "solves the problem"
      (expect (day14/part-1 (advent/read-problem-lines 14 :problem))
              :to-be 107142)))
  (xdescribe "part 2"
    (it "replicates the example"
      (expect (day14/part-2 (advent/read-problem-lines 14 :example))
              :to-be 64))
    (xit "solves the problem"
      (expect (day14/part-2 (advent/read-problem-lines 14 :problem))
              :to-be 104815))))
