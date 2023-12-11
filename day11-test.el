(require 'day11)
(require 'buttercup)

(defun day11/-to-symbol (s)
  (intern (concat ":" s)))

(describe "--- Day 11: Cosmic Expansion ---"
  (describe "part 1"
    (it "replicates the example"
      (expect (day11/part-1 (advent/read-grid 11 :example #'day11/-to-symbol))
              :to-be 374))
    (it "solves the problem"
      (expect (day11/part-1 (advent/read-grid 11 :problem #'day11/-to-symbol))
              :to-be 9957702)))
  (describe "part 2"
    (it "computes the distances in the example  with a 10x magnification"
      (expect (day11/all-distances 10 (day11/analyze-space (day11/read-data (advent/read-grid 11 :example #'day11/-to-symbol))))
              :to-be 1030))
    (it "computes the distances in the example  with a 100x magnification"
      (expect (day11/all-distances 100 (day11/analyze-space (day11/read-data (advent/read-grid 11 :example #'day11/-to-symbol))))
              :to-be 8410))
    (it "computes the distances in the example  with a 1000000x magnification"
      (expect (day11/part-2 (advent/read-grid 11 :example #'day11/-to-symbol))
              :to-be 82000210))
    (it "solves the problem"
      (expect (day11/part-2 (advent/read-grid 11 :problem #'day11/-to-symbol))
              :to-be 512240933238))))
