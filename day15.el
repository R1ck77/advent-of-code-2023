(require 'dash)
(require 'advent-utils)
(require 's)

(defun day15/read-data (lines)
  (s-split "," (car lines) t))

(defun day15/compute-hash (s)
  (--reduce-from (% (* (+ acc it) 17) 256) 0  (string-to-list s)))

(defun day15/compute-total-hash (xs)
  (apply #'+ (-map #'day15/compute-hash xs)))

(defun day15/part-1 (lines)
  (day15/compute-total-hash (day15/read-data lines)))

(defun day15/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day15)
