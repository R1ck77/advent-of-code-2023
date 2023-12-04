(require 'dash)
(require 'advent-utils)
(require 's)

(defun day04/-read-numbers-list (token)
  (-map #'string-to-number (s-split " " token t)))

(defun day04/-read-given-numbers (token)
  (day04/-read-numbers-list token))

(defun day04/-read-winning-numbers (token)
  (day04/-read-numbers-list token))

(defun day04/-read-card-number (token)
  (string-to-number
   (elt (s-match "Card +\\([0-9]+\\)" token) 1)))

(defun day04/-split-tokens (line)
  (-map #'s-trim (s-split "|" (s-replace ":" "|" line))))

(defun day04/read-data (lines)
  (--map (list
          (1- (day04/-read-card-number (elt it 0)))
          (day04/-read-winning-numbers (elt it 1))
          (day04/-read-given-numbers (elt it 2)))
         (-map #'day04/-split-tokens lines)))

(defun day04/-find-winning-numbers (card)
  (cl-intersection (elt card 1)
                   (elt card 2)))

(defun day04/-compute-score (n)
  (ash 1 (1- n)))

(defun day04/part-1 (lines)
  (apply #'+
         (-map #'day04/-compute-score
               (-map #'length
                     (-map #'day04/-find-winning-numbers
                           (day04/read-data lines))))))

(defun day04/-increase-owned (owned id winners)
  (let ((factor (aref owned id)))
    (--each (number-sequence (1+ id) (+ id winners))
      (aset owned it (+ factor (aref owned it))))))


(defun day04/-compute-owned (card-specs)
  (let* ((owned (make-vector (length card-specs) 1)))
    (--each card-specs
      (day04/-increase-owned owned
                             (car it) 
                             (length (day04/-find-winning-numbers it))))
    owned))

(defun day04/part-2 (lines)
  (apply #'+ (advent/v->l
              (day04/-compute-owned
               (day04/read-data lines)))))

(provide 'day04)
