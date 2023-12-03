(require 'dash)
(require 'advent-utils)
(require 's)

(defconst day03/num-regex "[0-9]+")
(defconst day03/symbol-regex "[^0-9 ]")

(defun day03/intersects? (column n-coord s-coord)
  )

(defun day03/read-numbers (row current-row)
  (let ((matches)
        (current-match))
    (while (setq current-match (car (s-match day03/num-regex current-row)))
      (let* ((start (s-index-of current-match current-row))
             (end (+ start (length current-match))))
        (push (list (string-to-number current-match)
                    row
                    (list start end))
              matches)
        (setq current-row (concat (substring current-row 0 start)
                                  (s-repeat (- end start) " ")
                                  (substring current-row end)))))
    (list current-row matches)))

;; TODO/FIXME horrible repeated code
(defun day03/read-symbols (row current-row)
  (let ((matches)
        (current-match))
    (while (setq current-match (car (s-match day03/symbol-regex current-row)))
      (let* ((start (s-index-of current-match current-row))
             (end (1+ start)))
        (push (list (read (concat ":" current-match))
                    row
                    start)
              matches)
        (setq current-row (concat (substring current-row 0 start)
                                  (s-repeat (- end start) " ")
                                  (substring current-row end)))))
    (list current-row matches)))

(defun day03/read-line (row s)
  (let ((current-row+matches (day03/read-numbers row (s-replace "." " " s))))
    (reverse (append (cadr (day03/read-symbols row (car current-row+matches))) (cadr current-row+matches)))))


(defun day03/part-1 (lines)
  (error "Not yet implemented"))

(defun day03/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day03)
