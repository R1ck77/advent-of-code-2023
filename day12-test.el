(require 'day12)
(require 'buttercup)

(describe "--- Day 12: Hot Springs ---"
  (describe "part 1"
    (it "computes the right arrangements for '???.### 1,1,3'"
      (expect (day12/get-arrangements (day12/read-line "???.### 1,1,3"))
              :to-be 1))
    (it "computes the right arrangements for '.??..??...?##. 1,1,3'"
      (expect (day12/get-arrangements (day12/read-line ".??..??...?##. 1,1,3"))
              :to-be 4))
    (it "computes the right arrangements for '?#?#?#?#?#?#?#? 1,3,1,6'"
      (expect (day12/get-arrangements (day12/read-line "?#?#?#?#?#?#?#? 1,3,1,6"))
              :to-be 1))
    (it "computes the right arrangements for '????.#...#... 4,1,1'"
      (expect (day12/get-arrangements (day12/read-line "????.#...#... 4,1,1"))
              :to-be 1))
    (it "computes the right arrangements for '????.######..#####. 1,6,5'"
      (expect (day12/get-arrangements (day12/read-line "????.######..#####. 1,6,5"))
              :to-be 4))
    (it "computes the right arrangements for '?###???????? 3,2,1'"
      (expect (day12/get-arrangements (day12/read-line "?###???????? 3,2,1"))
              :to-be 10))
    (it "replicates the example"
      (expect (day12/part-1 (advent/read-problem-lines 12 :example))
              :to-be 21))
    (xit "solves the problem"
      (expect (day12/part-1 (advent/read-problem-lines 12 :problem))
              :to-be 42)))
  (xdescribe "part 2"
    (it "replicates the example"
      (expect (day12/part-2 (advent/read-problem-lines 12 :example))
              :to-be 42))
    (xit "solves the problem"
      (expect (day12/part-2 (advent/read-problem-lines 12 :problem))
              :to-be 42))))
