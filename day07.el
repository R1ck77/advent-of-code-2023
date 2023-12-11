(require 'dash)
(require 'advent-utils)
(require 's)

(defconst day07/cards (reverse '(:A :K :Q :J :T :9 :8 :7 :6 :5 :4 :3 :2)))
(defconst day07/joker-cards (reverse '(:A :K :Q :T :9 :8 :7 :6 :5 :4 :3 :2 :J)))

(defun day07/read-bid (bid-string)
  (string-to-number bid-string))

(defun day07/read-hand (hand-string)
  (--map (read (concat ":" it))
         (s-split "" hand-string t)))

(defun day07/read-data (lines)
  (--map (list :hand (day07/read-hand (car it))
               :bid (day07/read-bid (cadr it)))
         (--map (s-split " " it) lines)))

(defun day07/bin-values (hand)
  (let ((bins (advent/table)))
    (--each hand
      (advent/update bins it (lambda (key old-value)
                               (if old-value
                                   (1+ old-value)
                                 1))))
    bins))

(defun day07/-subtract-values (values-a values-b)
  (--filter (not (zerop it))
            (--map (apply #'- it)
                   (-zip-lists values-a values-b))))

(defun day07/get-card-values (hand cards-index)
  (--map (-elem-index it cards-index) hand))


(defun day07/compare-value-scores (hand-a hand-b cards-index)
  (let ((values-a (day07/get-card-values hand-a cards-index))
        (values-b (day07/get-card-values hand-b cards-index)))    
    (>= (or (car (day07/-subtract-values values-a values-b)) 0) 0)))

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
     (t (day07/compare-value-scores hand-a hand-b day07/cards)))))

(defun day07/compare-plays (play-a play-b)
  (day07/compare-hands (plist-get play-a :hand)
                       (plist-get play-b :hand)))

(defun day07/joker-score-type (hand)
  (let* ((bins (day07/bin-values hand))
         (jokers (advent/get bins :J 0)))
    (remhash :J bins)
    (if (= jokers 5)
        (expt 10 5)      
      (let ((ordered-mult (apply #'vector (-sort #'> (advent/-map-hash bins
                                                       it-value)))))
        (aset ordered-mult 0 (+ jokers (aref ordered-mult 0)))
        (--reduce-from (+ acc (expt 10 it))
                       0
                       (advent/v->l ordered-mult))))))


(defun day07/joker-compare-hands (hand-a hand-b)
  "Returns thruty if the elements are in order"
  (let ((type-score-a (day07/joker-score-type hand-a))
        (type-score-b (day07/joker-score-type hand-b)))
    (cond 
     ((> type-score-a type-score-b) t)
     ((< type-score-a type-score-b) nil)
     (t (day07/compare-value-scores hand-a hand-b day07/joker-cards)))))

(defun day07/joker-compare-plays (play-a play-b)
  (day07/joker-compare-hands (plist-get play-a :hand)
                              (plist-get play-b :hand)))

(defun day07/total-winnings (plays sort-f)
  (apply #'+ (--map-indexed (* (plist-get it :bid) (1+ it-index))
                            (reverse (sort plays sort-f)))))

(defun day07/part-1 (lines)
  (day07/total-winnings (day07/read-data lines)
                        #'day07/compare-plays))

(defun day07/part-2 (lines)
  (day07/total-winnings (day07/read-data lines)
                        #'day07/joker-compare-plays))

(provide 'day07)
