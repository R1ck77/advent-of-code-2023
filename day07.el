(require 'dash)
(require 'advent-utils)
(require 's)

(defconst day07/cards (reverse '(:A :K :Q :J :T :9 :8 :7 :6 :5 :4 :3 :2)))

(defun day07/read-bid (bid-string)
  (string-to-number bid-string))

(defun day07/read-hand (hand-string)
  (--map (read (concat ":" it))
         (s-split "" hand-string t)))

(defun day07/read-data (lines)
  (--map (list :hand (day07/read-hand (car it))
               :bid (day07/read-bid (cadr it)))
         (--map (s-split " " it) lines)))

(defconst example (day07/read-data (advent/read-problem-lines 7 :example)))
(defconst problem (day07/read-data (advent/read-problem-lines 7 :problem)))

;;;(defun day07/compare-lists (a b comparator-f)
;;;  "Returns t if a is less or equal b.
;;;
;;;comparator-f is used on each pair, returns 0 if the pairs are the
;;;same, -1 if a is greater and 1 if b is greater"
;;;  (if (equal a b) t
;;;    ;; TODO/FIXME Not very optimized… It compares everything, but whathever
;;;    (let* ((comparisons (--map (apply comparator-f it) (-zip-lists a b)))
;;;           (result (car (--remove (zerop it) comparisons))))
;;;      (< result 1))))
;;;
;;;(defun day07/sign (a)
;;;  (if (zerop a)
;;;      0
;;;    (/ a (abs a))))
;;;
;;;(defun day07/frequency-comparator (a b)
;;;  (day07/sign (- a b)))
;;;
(defun day07/bin-values (hand)
  (let ((bins (advent/table)))
    (--each hand
      (advent/update bins it (lambda (key old-value)
                               (if old-value
                                   (1+ old-value)
                                 1))))
    bins))



(defun day07/compare-value-scores (hand-a hand-b)
  ;; TODO/FIXME split: Horrible
  (>= (or (car (--filter (not (zerop it))
                      (--map (apply #'- it)
                             (-zip-lists (--map (-elem-index it day07/cards) hand-a)
                                         (--map (-elem-index it day07/cards) hand-b)))))
          0)
      0))

(defun day07/score-type (hand)
  (--reduce-from (+ acc (expt 10 it))
                 0
                 (-sort #'> (advent/-map-hash (day07/bin-values hand)
                              it-value))))

(defun day07/compare-hands (hand-a hand-b)
  "Returns thruty if the elements are in order"
  (let ((type-score-a (day07/score-type hand-a))
        (type-score-b (day07/score-type hand-b)))
    (cond 
     ((> type-score-a type-score-b) t)
     ((< type-score-a type-score-b) nil)
     (t (day07/compare-value-scores hand-a hand-b)))))

(defun day07/compare-plays (play-a play-b)
  (day07/compare-hands (plist-get play-a :hand)
                        (plist-get play-b :hand)))

(defun day07/-sort-plays (plays)
  (sort plays #'day07/compare-plays))

(defun day07/total-winnings (plays)
  (apply #'+ (--map-indexed (* (plist-get it :bid) (1+ it-index))
                            (reverse (day07/-sort-plays plays)))))

(defun day07/part-1 (lines)
  (day07/total-winnings (day07/read-data lines)))

(defun day07/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day07)
