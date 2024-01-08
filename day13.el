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

(defun day13/get-total-score (grids f)
  (apply #'+ (-map f grids)))

(defun day13/part-1 (line-blocks)
  (day13/get-total-score (day13/read-data line-blocks)
                         #'day13/get-symmetry-score))

(defun day13/with-swapped-grid (grid row-column f &rest other)
  (advent/grid-set! grid row-column (not (advent/grid-get! grid/r))))

(defmacro day13/with-changed-grid (grid row-column &rest body)
  (let ((orig-value (make-symbol "orig-value"))
        (result (make-symbol "result"))
        (grid-symbol (make-symbol "grid"))
        (row-column-symbol (make-symbol "row-column")))
    `(let ((,orig-value (advent/grid-get ,grid ,row-column))
           (,result)
           (,grid-symbol ,grid)
           (,row-column-symbol ,row-column))
       (advent/grid-set! ,grid-symbol
                         ,row-column-symbol
                         (not ,orig-value))
       (setq ,result (progn ,@body))
       (advent/grid-set! ,grid-symbol
                         ,row-column-symbol
                         ,orig-value)
       ,result)))

(defun day13/get-all-coords (grid)
  (let ((size (advent/get-grid-size grid))
        (coords))
    (cl-loop for row below (car size) do
             (cl-loop for column below (cdr size) do
                      (push (cons row column) coords)))
    (reverse coords)))

(defun day13/get-parallel-symmetry-point (grid tgrid)
  (let ((result (list (day13/get-symmetry-point grid)
                      (day13/get-symmetry-point tgrid))))
    (if (equal result '(nil nil)) nil result)))

(defun day13/subtract-solutions (orig-solution new-solution)
  (list (car (-difference (car new-solution) (car orig-solution)))
        (car (-difference (cadr new-solution) (cadr orig-solution)))))

(defun day13/get-changed-score (original-grid)
  (let ((coords (day13/get-all-coords original-grid))
        (grid (advent/copy-grid original-grid))
        (tgrid (advent/transpose original-grid)))
    (let* ((original-solution (day13/get-parallel-symmetry-point grid tgrid))
           (solution original-solution)
           (coord))
      (while (and (equal original-solution solution) (setq coord (pop coords)))
        (let ((point-solution (day13/with-changed-grid grid 
                                                       coord
                                                       (day13/with-changed-grid tgrid (cons (cdr coord) (car coord))
                                                                (day13/get-parallel-symmetry-point grid tgrid)))))
          (when (and point-solution (not (equal point-solution solution)))
            (setq solution point-solution))))
      (apply (lambda (a b) (+ (or a 0) (* 100 (or b 0))))
             (day13/subtract-solutions original-solution solution)))))

(defun day13/part-2 (line-blocks)
  (day13/get-total-score (day13/read-data line-blocks)
                         #'day13/get-changed-score))

(provide 'day13)
