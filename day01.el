(require 'dash)
(require 'advent-utils)
(require 's)

;; Zero doesn't seem to be present…
(defconst string-to-number '(("one" 1)
                             ("two" 2)
                             ("three" 3)
                             ("four" 4)
                             ("five" 5)
                             ("six" 6)
                             ("seven" 7)
                             ("eight" 8)
                             ("nine" 9)
                             ("0" 0)
                             ("1" 1)
                             ("2" 2)
                             ("3" 3)
                             ("4" 4)
                             ("5" 5)
                             ("6" 6)
                             ("7" 7)
                             ("8" 8)
                             ("9" 9)))

(defun day01/-get-numbers (s)
  (-map #'string-to-number
        (-map #'char-to-string
              (--filter (and (> it 47)
                             (< it 58))
                        (--map it s)))))

(defun day01/-get-extremes (l)
  (list (car l) (car (reverse l))))

(defun day01/-number-from-extremes (pair)
  (+ (* 10 (car pair))
     (cadr pair)))

(defun day01/-add-all (l)
  (apply #'+ l))

(defun day01/part-1 (lines)
  (day01/-add-all
   (-map #'day01/-number-from-extremes
         (-map #'day01/-get-extremes
               (-map #'day01/-get-numbers lines)))))

;; 53867 low
;; 53903 wrong

(defun day01/-get-all-digits-matches (line)
  (--filter (car it) (--map (list (s-index-of (car it) line) (cadr it))
                            string-to-number))  )

(defun day01/-get-first-digit (line)
  (cadar (--sort (< (car it) (car other))
                 (day01/-get-all-digits-matches line))))

(defun day01/-get-last-digit (line)
  (cadar (--sort (> (car it) (car other))
                 (day01/-get-all-digits-matches line))))


(defun day01/get-pairs (line)
  (list (day01/-get-first-digit line)
        (day01/-get-last-digit line)))

(defun day01/part-2 (lines)
  (day01/-add-all
   (-map #'day01/-number-from-extremes
         (-map #'day01/get-pairs lines))))

(provide 'day01)
