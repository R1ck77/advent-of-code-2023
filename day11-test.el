(require 'day11)
(require 'buttercup)

(defun day11/-to-symbol (s)
  (intern (concat ":" s)))

(describe "--- Day 11: Cosmic Expansion ---"
  (describe "part 1"
    (it "replicates the example"
      (expect (day11/part-1 (advent/read-grid 11 :example #'day11/-to-symbol))
              :to-be 374))
    (xit "solves the problem"
      (expect (day11/part-1 (advent/read-grid 11 :problem #'day11/-to-symbol))
              :to-be 42)))
  (xdescribe "part 2"
    (it "replicates the example"
      (expect (day11/part-2 (advent/read-grid 11 :example #'day11/-to-symbol))
              :to-be 42))
    (xit "solves the problem"
      (expect (day11/part-2 (advent/read-grid 11 :problem #'day11/-to-symbol))
              :to-be 42))))
