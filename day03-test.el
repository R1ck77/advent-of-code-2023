(require 'day03)
(require 'buttercup)

(describe "--- Day 3: Gear Ratios ---"
  (describe "part 1"
    (it "reads the problem correctly"
      (expect (day03/read-line 0 "467..114..")
              :to-equal (list :row 0
                              :numbers '((467 0 (0 3))
                                        (114 0 (5 8)))
                              :symbols nil))
      (expect (day03/read-line 1 "...*......")
              :to-equal (list :row 1
                              :numbers nil
                              :symbols '((:* 1 3))))
      (expect (day03/read-line 4 "617*......")
              :to-equal (list :row 4
                              :numbers '((617 4 (0 3)))
                              :symbols '((:* 4 3))))
      (expect (day03/read-line 27 ".......755")
              :to-equal (list :row 27 :numbers '((755 27 (7 10)))
                              :symbols nil)))
    (it "detects intersections correctly"
      (expect (day03/intersects? '(0 (0 3))
                                 '(0 3))
              :to-be-truthy)
      (expect (day03/intersects? '(0 (5 8))
                                 '(0 3))
              :not :to-be-truthy))
    (it "replicates the example"
      (expect (day03/part-1 (advent/read-problem-lines 3 :example))
              :to-be 4361))
    (it "solves the problem"
      (expect (day03/part-1 (advent/read-problem-lines 3 :problem))
              :to-be 531932)))
  (describe "part 2"
    (it "replicates the example"
      (expect (day03/part-2 (advent/read-problem-lines 3 :example))
              :to-be 467835))
    (it "solves the problem"
      (expect (day03/part-2 (advent/read-problem-lines 3 :problem))
              :to-be 73646890))))
