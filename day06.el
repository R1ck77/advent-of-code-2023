(require 'dash)
(require 'advent-utils)
(require 's)

(defun day06/distance (total-time push-time)
  (let ((move-time (- total-time push-time)))
    (* (max 0 move-time) push-time)))

(defun day06/-read-line (line)  
  (-map #'string-to-number
        (s-split " "
                 (cadr (s-split ":" line t)) t)))

(defun day06/read-data (lines)
  (-zip (day06/-read-line (car lines))
        (day06/-read-line (cadr lines))))


(defun day06/-find-random-winning-solution (time+record)
  "Get a random push time that beats the record time.

Fast as long as the solutions are not too sparse in the total
time range (which appears to be the case, at least with my data)"
  (let ((total-time (car time+record))
        (record-distance (cdr time+record))
        (winning-push-time 0))
    (while (<= (day06/distance total-time winning-push-time)
               record-distance)
      (setq winning-push-time (random total-time)))
    winning-push-time))

(defun day06/find-first-solution (time+record random-solution)
  (let ((total-time (car time+record))
        (record-distance (cdr time+record)))
    (advent/bisect 1
                   random-solution
                   (lambda (push-time)
                     (> (day06/distance (car time+record) push-time)
                        (cdr time+record))))))


(defun day06/find-last-solution (time+record random-solution)
  (let ((total-time (car time+record))
        (record-distance (cdr time+record)))
    (advent/bisect random-solution
                   total-time
                   (lambda (push-time)
                     (<= (day06/distance (car time+record) push-time)
                         (cdr time+record))))))

(defun day06/find-extremes (time+record)
  (let ((random-solution (day06/-find-random-winning-solution time+record)))
    (list (day06/find-first-solution time+record random-solution)
          (day06/find-last-solution time+record random-solution))))

(defun day06/solve-problem (time+record-list)
  (apply #'*
         (--map (apply #'- (reverse it))
                (-map #'day06/find-extremes
                      time+record-list))))

(defun day06/part-1 (lines)
  (day06/solve-problem (day06/read-data lines)))

(defun day06/part-2 (lines)
  (day06/solve-problem (day06/read-data (--map (s-replace " " "" it) lines))))

(provide 'day06)
