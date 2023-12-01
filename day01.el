(require 'dash)
(require 'advent-utils)
(require 's)

;; Zero doesn't seem to be present…
(defconst string-to-digit '(("one" "1")
                            ("two" "2")
                            ("three" "3")
                            ("four" "4")
                            ("five" "5")
                            ("six" "6")
                            ("seven" "7")
                            ("eight" "8")
                            ("nine" "9")))

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

(defun day01/-next-replacement (line)
  (caadar (--sort (< (car it) (car other))
                  (--filter (car it)
                            (--map (list (s-index-of (car it) line)
                                         it)
                                   string-to-digit)))))

(defun day01/-fuck-elisp (text start end replacement)
  (let ((head (substring text 0 start))
        (tail (substring text end)))
    (concat head replacement tail)))

(defun day01/replace-english-numbers (line)  
  ;; fist attempt: just replace the numbers in order  
  (let ((to-replace  (day01/-next-replacement line)))
    (while to-replace
      (let* ((replacement (cadr (assoc to-replace string-to-digit)))
            (start (s-index-of to-replace line))
            (end (+ start (length to-replace))))
        (setq line (day01/-fuck-elisp line start end replacement))
        (setq to-replace (day01/-next-replacement line))))
    line))

(defun day01/-replace-numerical-strings (lines)
  (-map #'day01/replace-english-numbers lines))

(defun day01/part-2 (lines)
  (day01/part-1
   (day01/-replace-numerical-strings lines)))

(provide 'day01)
