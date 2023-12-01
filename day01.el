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
;; 53885 wrong
;; 53903 wrong

(defun day01/-get-match-data (line repr+value)
  (let ((pattern (car repr+value)))
   (when-let ((match-start (s-index-of pattern line)))
     (list match-start (cadr repr+value) (substring line (+ match-start (length pattern)))))))

(defun day01/-get-first-match (line)
  (rest (car (--sort (< (car it) (car other))
                (--filter (car it)
                          (--map (day01/-get-match-data line it)
                                 string-to-number))))))

(defun day01/-get-all-digits (line)
  (let ((numbers)
        (remaining line)
        (match))
    (while remaining
      (setq match (day01/-get-first-match remaining))
      (if match
          (progn
            (setq numbers (cons (car match) numbers))
            (setq remaining (cadr match)))
        (setq remaining nil)))
    (reverse numbers)))


(defun day01/get-pairs (line)
  (let ((digits (day01/-get-all-digits line)))
    (list (car digits) (car (reverse digits))))
)

(defun day01/part-2 (lines)
  (day01/-add-all
   (-map #'day01/-number-from-extremes
         (-map #'day01/get-pairs lines))))

(provide 'day01)
