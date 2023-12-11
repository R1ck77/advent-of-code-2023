(require 'dash)
(require 'advent-utils)
(require 's)

(defun day11/read-data (grid)
  (let ((stars))
    (advent/-each-grid grid
      (if (eq it-value :\#)
          (push it-coord stars)))
    stars))

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

(defun day11/count-unexpanded-space (stars from to)
  (length
   (--filter (and (>= it from)
                  (<= it to))
             (advent/-map-hash stars it-key))))

(defun day11/create-unexpanded-space-f (stars-by-coord)
  (lexical-let ((stars stars-by-coord))
    (advent/cache-f (lambda (a-b)
                      (day11/count-unexpanded-space stars
                                                    (car a-b)
                                                    (cdr a-b))))))

(defun day11/analyze-space (stars)
  (list :stars stars
        :star-rows-f (day11/create-unexpanded-space-f (day11/find-star-rows stars))
        :star-columns-f (day11/create-unexpanded-space-f (day11/find-star-columns stars))))

(defun day11/a-b-distance (factor a b stars-f)
  (if (= a b) 0
    (let* ((from (min a b))
           (to (max a b))
           (distance (1+ (- to from)))
           (unexpanded-tiles (funcall stars-f (cons from to)))
           (expanded-tiles (- distance unexpanded-tiles)))
      (1- (+ unexpanded-tiles (* factor expanded-tiles))))))

(defun day11/find-distance (factor data star-a star-b)
  "Distance between two stars."
  (let ((vertical-distance (day11/a-b-distance factor (car star-a) (car star-b) (plist-get data :star-rows-f)))
        (horizontal-distance (day11/a-b-distance factor (cdr star-a) (cdr star-b) (plist-get data :star-columns-f))))
    (+ vertical-distance horizontal-distance)))

(defun day11/all-distances (factor data)
  (let ((sum 0)
        (stars (plist-get data :stars)))
    (while stars
      (--each (rest stars)
        (setq sum (+ sum (day11/find-distance factor data (car stars) it))))
      (pop stars))
    sum))

(defun day11/part-1 (grid)
  (day11/all-distances 2 (day11/analyze-space (day11/read-data grid))))

(defun day11/part-2 (grid)
  (day11/all-distances 1000000 (day11/analyze-space (day11/read-data grid))))

(provide 'day11)
