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

(defun day06/part-2 (lines)
  (apply #'*
         (-map #'day06/compute-win-chances
               (day06/read-data (--map (s-replace " " "" it) lines)))))

(provide 'day06)
