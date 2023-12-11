(require 'day10)
(require 'buttercup)

(defun day10/-to-symbol (s)
  (intern (concat ":" s)))

(describe "--- Day 10: Pipe Maze ---"
  (describe "part 1"
    (it "replicates the example"
      (expect (day10/part-1 (advent/read-grid 10 :example #'day10/-to-symbol))
              :to-be 8))
    (xit "solves the problem"
      (expect (day10/part-1 (advent/read-grid 10 :problem #'day10/-to-symbol))
              :to-be 42)))
  (xdescribe "part 2"
    (it "replicates the example"
      (expect (day10/part-2 (advent/read-grid 10 :example #'day10/-to-symbol))
              :to-be 42))
    (xit "solves the problem"
      (expect (day10/part-2 (advent/read-grid 10 :problem #'day10/-to-symbol))
              :to-be 42))))
