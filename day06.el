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

(defun day06/compute-win-chances (time+record)
  (let ((total-time (car time+record))
        (record-distance (cdr time+record)))
    (length
     (--filter (> it record-distance)
               (--map (day06/distance total-time it)
                      (number-sequence 0 total-time))))))

(defun day06/part-1 (lines)
  (apply #'*
         (-map #'day06/compute-win-chances
               (day06/read-data lines))))

(defun day06/-find-random-winning-solution (time+record)
  (let ((total-time (car time+record))
        (record-distance (cdr time+record))
        (winning-push-time 0))
    (while (<= (day06/distance total-time winning-push-time)
               record-distance)
      (setq winning-push-time (random total-time)))
    winning-push-time))

(defun day06/-bisect-step (from to f)
;  (message (format "Bisecting %s (%s) %s (%s)…" from (funcall f from) to (funcall f to)))
  (unless (funcall f to)
    (error (format "Invalid extreme %s" to)))
  (cond
   ((= from to)
    (list from nil))
   ((funcall f from)
    (list from nil))
   ((= (1+ from) to)
    (list to nil))
   (t (let* ((half-point (/ (+ from to) 2))
           (half-result (funcall f half-point)))
      (if half-result
          (list nil from half-point)
        (list nil half-point to))))))

;; TODO/FIXME move toe 
(defun day06/bisect (from to f)
  "Returns the first value in [from to] where f changes from nil to a truthy value.

It's supposed to work on integers

As expected, if there are multiple changes of 'sign', the algorithm will return a non specified one"
;  (message (format "Bisecting %s (%s) %s (%s)…" from (funcall f from) to (funcall f to)))
  (let ((current-result ))
    (while (not (car (setq current-result (day06/-bisect-step from to f))))      
      (setq from (elt current-result 1))
      (setq to (elt current-result 2)))
    (car current-result)))

(defun day06/find-first-solution (time+record random-solution)
  (let ((total-time (car time+record))
        (record-distance (cdr time+record)))
    (day06/bisect 1
                   random-solution
                   (lambda (push-time)
                     (> (day06/distance (car time+record) push-time)
                        (cdr time+record))))))

(defun day06/find-last-solution (time+record random-solution)
  (let ((total-time (car time+record))
        (record-distance (cdr time+record)))
    (day06/bisect random-solution
                  total-time
                  (lambda (push-time)
                    (<= (day06/distance (car time+record) push-time)
                        (cdr time+record))))))

(defun day06/find-extremes (time+record)
  (let ((random-solution (day06/-find-random-winning-solution time+record)))
    (list (day06/find-first-solution time+record random-solution)
          (day06/find-last-solution time+record random-solution))))

(defun day06/part-2 (lines)
  (apply #'*
         (--map (apply #'- (reverse it))
                (-map #'day06/find-extremes
                         (day06/read-data (--map (s-replace " " "" it) lines))))))

(provide 'day06)
