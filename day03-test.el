(require 'day03)
(require 'buttercup)

(describe "--- Day 3: Gear Ratios ---"
  (describe "part 1"
    (it "reads the problem correctly"
      (expect (day03/read-line 0 "467..114..")
              :to-equal '((467 (0 0) (0 3))
                          (114 (0 5) (0 8))))
      (expect (day03/read-line 1 "...*......")
              :to-equal '((:* (0 3) (0 4))))
      (expect (day03/read-line 4 "617*......")
              :to-equal '((617 (4 0) (4 3))
                          (:* (4 4) (4 5))))
      (expect (day03/read-line 27 ".......755")
              :to-equal '((755 (27 7) (27 10)))))
    (it "detects intersections correctly"
      (expect (day03/intersects? '((0 0) (0 3))
                                 '((0 3 (0 4))))
              :to-be-truthy)
      (expect (day03/intersects? '((0 5) (0 8))
                                 '((0 3 (0 4))))
              :not :to-be-truthy))
    (it "replicates the example"
      (expect (day03/part-1 (advent/read-problem-lines 3 :example))
              :to-be 4361))
    (xit "solves the problem"
      (expect (day03/part-1 (advent/read-problem-lines 3 :problem))
              :to-be 42)))
  (xdescribe "part 2"
    (it "replicates the example"
      (expect (day03/part-2 (advent/read-problem-lines 3 :example))
              :to-be 42))
    (xit "solves the problem"
      (expect (day03/part-2 (advent/read-problem-lines 3 :problem))
              :to-be 42))))
