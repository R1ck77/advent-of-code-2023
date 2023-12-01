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

(defun day01/-next-replacement ()
  (cadr
   (car
    (--sort (< (car it )
               (car other))
            (--filter it
                      (--map (progn
                               (goto-char (point-min))
                               (if (search-forward (car it) nil t)
                                   (list (match-beginning 0) it)))
                             string-to-digit))))))

(defun day01/-replace-all ()
  (let ((next-replacement (day01/-next-replacement)))
    (while next-replacement
      (goto-char (point-min))
      (search-forward (car next-replacement))
      (replace-match (cadr next-replacement))
      (setq next-replacement (day01/-next-replacement)))))

(defun day01/replace-english-numbers (line)
  (with-temp-buffer
    (insert line)
    (day01/-replace-all)
    (buffer-substring (point-min) (point-max))))


(defun day01/-replace-numerical-strings (lines)
  (-map #'day01/replace-english-numbers lines))

(defun day01/part-2 (lines)
  (day01/part-1
   (day01/-replace-numerical-strings lines)))

(provide 'day01)
