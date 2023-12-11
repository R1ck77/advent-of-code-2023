(require 'day10)
(require 'buttercup)

(defun day10/-to-symbol (s)
  (intern (concat ":" s)))

(describe "--- Day 10: Pipe Maze ---"
  (describe "part 1"
    (it "replicates the example"
      (expect (day10/part-1 (advent/read-grid 10 :example #'day10/-to-symbol))
              :to-be 8))
    (it "solves the problem"
      (expect (day10/part-1 (advent/read-grid 10 :problem #'day10/-to-symbol))
              :to-be 6754)))
  (describe "part 2"
    (it "replicates the example (example 1"
      (expect (day10/part-2 (advent/read-grid 10 :example #'day10/-to-symbol))
              :to-be 1))
    (it "replicates the example (example 2)"
      (expect (day10/part-2 (advent/read-grid 10 :example #'day10/-to-symbol 2))
              :to-be 4))
    (it "replicates the example (example 3)"
      (expect (day10/part-2 (advent/read-grid 10 :example #'day10/-to-symbol 3))
              :to-be 10))
    (it "replicates the example (example 4)"
      (expect (day10/part-2 (advent/read-grid 10 :example #'day10/-to-symbol 4))
              :to-be 8))
    (it "replicates the example (example 5)"
      (expect (day10/part-2 (advent/read-grid 10 :example #'day10/-to-symbol 5))
              :to-be 1))
    (it "replicates the example (example 6)"
      (expect (day10/part-2 (advent/read-grid 10 :example #'day10/-to-symbol 6))
              :to-be 0))    
    (xit "solves the problem"
      (expect (day10/part-2 (advent/read-grid 10 :problem #'day10/-to-symbol))
              :to-be 42))))
