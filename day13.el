(require 'dash)
(require 'advent-utils)
(require 's)

(defun day13/symbol-to-value (s)
  (cond 
   ((string= "#" s) t)
   ((string= "." s) nil)
   (t (error "Invalid symbol detected"))))

(defun day13/read-data (line-blocks)
  (--map (advent/lines-to-grid it #'day13/symbol-to-value) line-blocks))

(setq example (day13/read-data (advent/read-blocks-of-lines 13 :example)))
(setq problem (day13/read-data (advent/read-blocks-of-lines 13 :problem)))

(defun day13/is-symmetry-point? (values index)
  (cl-assert (> index 0))
  (cl-assert (< index (length values)))
  (--all? (eq (car it) (cdr it))
          (-zip (reverse (-take index values))
                (-drop index values))))

(defun day13/filter-symmetry-points (values indices)
  (--filter (day13/is-symmetry-point? values it) indices))

(defun day13/get-symmetry-point (grid)
  (let ((rows-columns (advent/get-grid-size grid))
        (table (advent/grid-to-table grid)))
    (--reduce-from (day13/filter-symmetry-points it acc )
                   (number-sequence 1 (1- (cdr rows-columns)))
                   table)))

(defun day13/get-or-0 (list)
  (or (car list) 0))

(defun day13/get-symmetry-score (grid)
  (+ (day13/get-or-0 (day13/get-symmetry-point grid))
     (* 100 (day13/get-or-0 (day13/get-symmetry-point (advent/transpose grid))))))

(defun day13/get-total-score (grids)
  (apply #'+ (-map #'day13/get-symmetry-score grids)))

(defun day13/part-1 (line-blocks)
  (day13/get-total-score (day13/read-data line-blocks)))

(defun day13/part-2 (line-blocks)
  (error "Not yet implemented"))

(provide 'day13)
