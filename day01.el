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
;; 53881 wrong

(defconst day01/replacements '(("one" "1")
                               ("two" "2")
                               ("three" "3")
                               ("four" "4")
                               ("five" "5")
                               ("six" "6")
                               ("seven" "7")
                               ("eight" "8")
                               ("nine" "9")))

(defun day01/-get-possible-replacements (line)
  "Returns a list of (start-position value replacement)"
  (--sort (< (car it) (car other))
          (--filter (car it)
                    (--map (cons (s-index-of (car it) line) it)
                           day01/replacements))))

(defun day01/-replace (line start+value+replacement)
  (if start+value+replacement
   (concat (substring line 0 (car start+value+replacement))
           (elt start+value+replacement 2)
           (substring line (+ (car start+value+replacement) (length (cadr start+value+replacement)))))
   line))

(defun day01/-replace-first (line)
  (day01/-replace line (car (day01/-get-possible-replacements line))))

(defun day01/-replace-last (line )
  (day01/-replace line (car (reverse (day01/-get-possible-replacements line)))))

(defun day01/replace-values (line)
  (day01/-replace-last (day01/-replace-first line)))

(defun day01/part-2 (lines)
  (day01/part-1
   (-map #'day01/replace-values lines)))

(provide 'day01)
