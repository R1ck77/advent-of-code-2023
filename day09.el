(require 'dash)
(require 'advent-utils)
(require 's)

(defun day09/-read-sequence (line)
  (-map #'string-to-number (s-split " " line)))

(defun day09/read-data (lines)
  (-map #'day09/-read-sequence lines))


(defun day09/-compute-subsequence (seq)
  (--map (apply #'- it)
         (-map #'reverse
               (-partition-in-steps 2 1 seq))))

(defun day09/is-zero-sequence? (seq)
  (-all? #'zerop seq))

(defun day09/compute-subsequences (seq)
  (let ((acc (list seq)))
    (while (not (day09/is-zero-sequence? (car acc)))
      (push (day09/-compute-subsequence (car acc)) acc))
    acc))

(defun day09/-next-increment (value seq)
  (+ value (car (reverse seq))))

(defun day09/compute-prediction (seq-list)
  (-reduce-from #'day09/-next-increment 0 seq-list))

(defun day09/part-1 (lines)
  (apply #'+ (-map #'day09/compute-prediction
                   (-map #'day09/compute-subsequences
                         (day09/read-data lines)))))

(defun day09/-next-inverse-increment (value seq)
  (- (car seq) value))

(defun day09/compute-inverse-prediction (seq-list)
  (-reduce-from #'day09/-next-inverse-increment 0 seq-list))

(defun day09/part-2 (lines)
  (apply #'+ (-map #'day09/compute-inverse-prediction
                   (-map #'day09/compute-subsequences
                         (day09/read-data lines)))))

(provide 'day09)
