(require 'dash)
(require 'advent-utils)
(require 's)

(defun day02/add-draw (draw-table color-string)
  (pcase (s-match "[0-9]+ \\(red\\|green\\|blue\\)" color-string)
    (`(,svalue ,color) (advent/put draw-table color (string-to-number svalue)))))

(defun day02/parse-draw (sdraw)
  (let ((draw-table (advent/table)))
    (--each (s-split "," sdraw)
      (day02/add-draw draw-table it))
    (list (advent/get draw-table "red" 0)
          (advent/get draw-table "green" 0)
          (advent/get draw-table "blue" 0))))

(defun day02/parse-draws (draws)
  "String of draws -> list of 3-tuples"
  (-map #'day02/parse-draw (s-split ";" draws)))

(defun day02/read-game (line)
  "Line -> id, followed by a list of 3 tuples"
  (pcase (s-match "^Game \\([0-9]+\\): \\(.*\\)" line)
    (`(,unused ,sid ,draws) (list (string-to-number sid)
                                  (day02/parse-draws draws)))))

(defun day02/read-games (lines)
  (-map #'day02/read-game lines))

(defun day02/is-valid-draw (draw max-cubes)
  (not (--filter (> (car it) (cdr it)) (-zip draw max-cubes))))

(defun day02/is-valid-game (game &optional max-cubes)
  (let ((max-cubes (or max-cubes '(12 13 14))))
    (--all? (day02/is-valid-draw it max-cubes) (cadr game))))

(defun day02/part-1 (lines)
  (apply #'+
         (-map #'car (-filter #'day02/is-valid-game
                        (day02/read-games lines)))))

(defun day02/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day02)
