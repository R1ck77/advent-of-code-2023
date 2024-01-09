(require 'dash)
(require 'advent-utils)
(require 's)

(defun day14/letter-to-symbol (s)
  (cond
   ((string= s "O") :O)
   ((string= s "#") :block)
   ((string= s ".") nil)
   (t (error "Unexpected letter!"))))

(defun day14/read-data (lines)
  (advent/lines-to-grid lines #'day14/letter-to-symbol))
;;
;;(setq example (advent/read-grid 14 :example #'day14/letter-to-symbol))
;;(setq problem (advent/read-grid 14 :problem #'day14/letter-to-symbol))
;;
(defun day14/move-rock-up! (grid coord)
  "Very slow and ill matched to the structure I'm using. I hope it won't matter"
  (let ((column (cdr coord))
        (stop)
        (steps 0))
    (cl-loop for row from (1- (car coord)) downto 0 unless stop do 
             (if (advent/grid-get grid (cons row column))
                 (setq stop t)
               (setq steps (1+ steps))))
    (unless (zerop steps)
      (advent/grid-set! grid coord nil)
      (advent/grid-set! grid (cons (- (car coord) steps) column) :O))))

(defun day14/move-everything-north (grid)
  "Warning: this function assumes that the -each-grid function moves by row…"
  (let ((new-grid (advent/copy-grid grid)))
   (advent/-each-grid new-grid
     (when (eq it-value :O)
       (day14/move-rock-up! new-grid it-coord)))
   new-grid))

(defun day14/weight-rocks (grid)
  (let ((rows (car (advent/get-grid-size grid)))
        (weight 0))
    (advent/-each-grid grid
      (if (eq it-value :O)
          (setq weight (+ weight (- rows (car it-coord))))))
    weight))

(defun day14/debug-grid-to-string (grid)
  (apply #'concat
         (-interpose "\n" (-map (lambda (v) (apply #'concat (--map (case it
                                                               (:block "#")
                                                               (:O "O")
                                                               (t "."))
                                                             (advent/v->l v))))
                          (advent/v->l grid)))))

(defun day14/part-1 (lines)
  (day14/weight-rocks
   (day14/move-everything-north
    (day14/read-data lines))))

(defun day14/part-2 (lines)
  (day14/read-data lines)
  (error "Not yet implemented"))

(provide 'day14)
