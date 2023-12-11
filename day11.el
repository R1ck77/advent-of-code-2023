(require 'dash)
(require 'advent-utils)
(require 's)




(defun day11/read-data (grid)
  (let ((stars))
   (advent/-each-grid grid
     (if (eq it-value :\#)
         (push it-coord stars)))
   stars))

(defun day11/-to-symbol (s)
  (intern (concat ":" s)))
(setq example (day11/read-data (advent/read-grid 11 :example #'day11/-to-symbol)))
(setq problem (day11/read-data (advent/read-grid 11 :problem #'day11/-to-symbol)))


(defun day11/find-star-coordinate (stars type)
  (let ((extract-f (if (eq type :row) #'car #'cdr))
        (table (advent/table)))
        (--each stars
          (advent/update table
                     (funcall extract-f it)
                     (lambda (key old-value &rest other)
                       (if old-value
                           (cons it old-value)
                         (list it)))))
        table))

(defun day11/find-star-columns (stars)
  (day11/find-star-coordinate stars :column))

(defun day11/find-star-rows (stars)
  (day11/find-star-coordinate stars :row))

(defun day11/analyze-space (stars)
  (list :stars stars
        :star-rows (day11/find-star-rows stars)
        :star-columns (day11/find-star-columns stars)))

(defun day11/count-unexpanded-space (stars from to)
  (length (--filter (and (>= it from)
                         (<= it to))  (advent/-map-hash stars it-key))))

(defun day11/a-b-distance (a b stars &optional factor)
  (if (= a b) 0
    (let* ((factor (or factor 2))
           (from (min a b))
           (to (max a b))
           (unexpanded-tiles (and 0 (day11/count-unexpanded-space stars from to))))
      (1+ (- (* (- to from) factor)
             (* (1- factor) unexpanded-tiles))))))

(defun day11/find-distance (data star-a star-b)
  "Distance between two stars."
  (let ((vertical-distance (day11/a-b-distance (car star-a) (car star-b) (plist-get data :star-rows)))
        (horizontal-distance (day11/a-b-distance (cdr star-a) (cdr star-b) (plist-get data :star-columns))))
    (comment
     (message "H:%s + V:%s -> %s"
              horizontal-distance
              vertical-distance
              (+ vertical-distance horizontal-distance)))
    (+ vertical-distance horizontal-distance)))

(defun day11/all-distances (data)
  (let ((sum 0)
        (stars (plist-get data :stars)))
    (while stars
      (--each (rest stars)
        (setq sum (+ sum (day11/find-distance data (car stars) it))))
      (pop stars))
    sum))

(defun day11/part-1 (grid)
  (day11/all-distances (day11/analyze-space (day11/read-data grid))))

(defun day11/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day11)
