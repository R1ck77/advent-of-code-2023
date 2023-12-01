(require 'day01)
(require 'buttercup)

(describe "--- Day 1: Trebuchet?! ---"
  (describe "part 1"
    (it "replicates the example"
      (expect (day01/part-1 (advent/read-problem-lines 1 :example))
              :to-be 142))
    (it "solves the problem"
      (expect (day01/part-1 (advent/read-problem-lines 1 :problem))
              :to-be 54953)))
  (describe "part 2"
    (it "replaces each string to numbers correctly"
      (expect (day01/get-pairs "two1nine") :to-equal '(2 9))
      (expect (day01/get-pairs "eightwothree") :to-equal '(8 3))
      (expect (day01/get-pairs "abcone2threexyz") :to-equal '(1 3))
      (expect (day01/get-pairs "xtwone3four") :to-equal '(2 4))
      (expect (day01/get-pairs "4nineeightseven2") :to-equal '(4 2))
      (expect (day01/get-pairs "zoneight234") :to-equal '(1 4))
      (expect (day01/get-pairs "7pqrstsixteen") :to-equal '(7 6)))
    (it "replicates the example"
      (expect (day01/part-2 (advent/read-problem-lines 1 :example 2))
              :to-be 281))
    (it "solves the problem"
      (expect (day01/part-2 (advent/read-problem-lines 1 :problem))
              :to-be 53903))))
