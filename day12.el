(require 'dash)
(require 'advent-utils)
(require 's)

(defun day12/read-digits (s)
  (-map #'string-to-number (s-split "," s)))

(defun day12/read-line (line)
  (let ((tokens (s-split " " line)))
    (list :s (car tokens)
          :digits (day12/read-digits (cadr tokens)))))

(defun day12/read-data (lines)
  (-map #'day12/read-line lines))

(setq example (day12/read-data (advent/read-problem-lines 12 :example)))
(setq problem (day12/read-data (advent/read-problem-lines 12 :problem)))

(defun day12/is-complete? (data)
  (not (s-contains? "?" (plist-get data :s))))

(defun day12/empty-to-nil (s)
  (unless (s-blank? s) s))

(defun day12/drop-last-spring (s)
  (s-reverse (cadr (s-match "#*\\(.*\\)" (s-reverse s)))))

(defun day12/get-useful-part (s)
  ;;; TODO/FIXME investigate plet better
  (pcase-let ((`(ignore ,start ,rest) (s-match "\\([^?]*\\)\\(.*\\)" s)))
    (unless (s-blank? start)
      (if (s-starts-with? "?" rest)
          (day12/empty-to-nil (day12/drop-last-spring start))
        start))))

(defun day12/find-springs (data)
  (if-let ((solved-part (day12/get-useful-part (plist-get data :s))))
      (-map #'length
            (s-split "[.]+" solved-part t))))

(defun day12/is-compatible? (data)
  ;;; Very very loose. "###########? 3" is not caught…
  (let ((score (day12/find-springs data))
        (digits (plist-get data :digits)))
    (if (day12/is-complete? data)
        (equal score digits)
;;; missing check for completeness!
      (unless (> (length score) (length digits))
        (--all? (eq (car it) (cdr it))
                (-zip score digits))))))

(defun day12/replace-at (s index new-value)
  (let ((new-string (copy-sequence s)))
    (aset new-string index (string-to-char new-value))
    new-string))

(defun days12/get-alternatives (data)
  (cl-assert (not (day12/is-complete? data)))
  (let ((s (plist-get data :s))
        (digits (plist-get data :digits)))
    (let ((first-? (s-index-of "?" s)))
      (cl-assert first-?)      
      (-filter #'day12/is-compatible?
               (--map (list :s it
                            :digits digits)
                      (list (day12/replace-at s first-? "#")
                            (day12/replace-at s first-? ".")))))))

(defun day12/find-combinations (data)
  (if (day12/is-complete? data)
      (list data)
    (let ((new-combinations (days12/get-alternatives data)))
      (apply #'append (-map #'day12/find-combinations new-combinations)))))

(defun day12/count-combinations (data)
  (length (day12/find-combinations data)))

(defun day12/sum-all-combinations (data)
  (--map (length (day12/find-combinations it)) data))


(defun day12/part-1 (lines)
  (apply #'+ (day12/sum-all-combinations (day12/read-data lines))))

(defun day12/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day12)
