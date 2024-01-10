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

(setq example (advent/read-grid 14 :example #'day14/letter-to-symbol))
(setq problem (advent/read-grid 14 :problem #'day14/letter-to-symbol))

(defun day14/move-rock-up! (grid coord)
  "VERY slow and ill matched for the structure I'm using.

In the end, however:

a) it'ss fast enough to be usable
b) I know by heart how I should optimize it
c) I have a life to live."
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

(defun day14/cycle! (grid)
  (advent/rotate-right
   (day14/move-everything-north
    (advent/rotate-right
     (day14/move-everything-north
      (advent/rotate-right
       (day14/move-everything-north
        (advent/rotate-right
         (day14/move-everything-north grid)))))))))

(defun day14/hash-grid (grid)
  (md5 (format "%s" grid)))

(defun day14/make-iterator (grid)
  (lexical-let ((copy (advent/copy-grid grid)))
    (lambda ()
      (let ((result (cons (day14/hash-grid copy)
                          (day14/weight-rocks copy))))
        (setq copy (day14/cycle! copy))
        result))))

(defun day14/create-history-function (grid)
  (lexical-let ((values)
                (f (day14/make-iterator grid)))
    (lambda (&optional i)
      (let ((i (or i (length values))))
       (while (<= (length values) i)
         (push (funcall f) values))
       (elt values (- (length values) i 1))))))

(defun day14/find-period-data (f)
  (let ((first)
        (period)
        (values))
    (while (not period)
      (push (car (funcall f)) values)
      (when (-contains? (cdr values) (car values))
        (let ((list (reverse values)))
          (setq first (-elem-index (car values) list))
          (setq period (- (length list) first 1)))))
    (list first period)))

(defun day14/compute-target-from-period-data (f first+period)
  (let* ((first (car first+period))
         (period (cadr first+period))
         (index (+ (% (- 1000000000 first) period) first)))
    (cdr (funcall f index))))

(defun day14/part-2 (lines)
  (let ((f (day14/create-history-function (day14/read-data lines))))
    (day14/compute-target-from-period-data f (day14/find-period-data f))))

(provide 'day14)
