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
              :to-be 9957702)))
  (describe "part 2"
    (it "computes the distance with a 10x magnification"
      (expect (day11/all-distances 100 (day11/analyze-space (day11/read-data (advent/read-grid 11 :example #'day11/-to-symbol))))
              :to-be 1030))
    (it "computes the distance with a 1000x magnification"
      (expect (day11/all-distances 1000 (day11/analyze-space (day11/read-data (advent/read-grid 11 :example #'day11/-to-symbol))))
              :to-be 8410))
    (xit "replicates the example"
      (expect (day11/part-2 (advent/read-grid 11 :example #'day11/-to-symbol))
              :to-be 42))
    (xit "solves the problem"
      (expect (day11/part-2 (advent/read-grid 11 :problem #'day11/-to-symbol))
              :to-be 42))))
