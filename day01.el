(require 'dash)
(require 'advent-utils)
(require 's)

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

(defconst day01/replacements '(("one" "1")
                               ("two" "2")
                               ("three" "3")
                               ("four" "4")
                               ("five" "5")
                               ("six" "6")
                               ("seven" "7")
                               ("eight" "8")
                               ("nine" "9")))

(defun day01/-get-ready-replacement (line)
  (car (--filter (s-starts-with? (car it) line) day01/replacements)))

(defun day01/replace-values (line)
  (let ((processed)
        (remaining line))
    (while (not (zerop (length remaining)))
      (let ((replacement (day01/-get-ready-replacement remaining)))
        (if replacement
            (progn
              (push (cadr replacement) processed)
              (setq remaining (substring remaining (length (car replacement))))
              )
          (push (substring remaining 0 1) processed)
          (setq remaining (substring remaining 1)))))
    (apply #'concat (reverse processed))))

(defun day01/part-2 (lines)
  (day01/part-1
   (-map #'day01/replace-values lines)))

(provide 'day01)
