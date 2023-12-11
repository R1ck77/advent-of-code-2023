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

(defun day09/compute-prediction (prediction-f seq-list)
  (-reduce-from prediction-f 0 seq-list))

(defun day09/compute-prediction-sequence (prediction-f seq-list)
  (--map (day09/compute-prediction prediction-f it)
         (-map #'day09/compute-subsequences seq-list)))

(defun day09/part-1 (lines)
  (apply #'+ (day09/compute-prediction-sequence
              #'day09/-next-increment
              (day09/read-data lines))))

(defun day09/-next-inverse-increment (value seq)
  (- (car seq) value))

(defun day09/part-2 (lines)
  (apply #'+ (day09/compute-prediction-sequence
              #'day09/-next-inverse-increment
              (day09/read-data lines))))

(provide 'day09)
