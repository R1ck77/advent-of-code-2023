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
      (expect (day01/replace-english-numbers "two1nine") :to-equal "219")
      (expect (day01/replace-english-numbers "eightwothree") :to-equal "8wo3")
      (expect (day01/replace-english-numbers "abcone2threexyz") :to-equal "abc123xyz")
      (expect (day01/replace-english-numbers "xtwone3four") :to-equal "x2ne34")
      (expect (day01/replace-english-numbers "4nineeightseven2") :to-equal "49872")
      (expect (day01/replace-english-numbers "zoneight234") :to-equal "z1ight234")
      (expect (day01/replace-english-numbers "7pqrstsixteen") :to-equal "7pqrst6teen"))
    (it "replicates the example"
      (expect (day01/part-2 (advent/read-problem-lines 1 :example 2))
              :to-be 281))
    (it "solves the problem"
      (expect (day01/part-2 (advent/read-problem-lines 1 :problem))
              :to-be 53885))))
