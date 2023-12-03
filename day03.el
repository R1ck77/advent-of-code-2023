(require 'dash)
(require 'advent-utils)
(require 's)

(defconst day03/num-regex "[0-9]+")
(defconst day03/symbol-regex "[^0-9 ]")

(defun day03/-create-coords (row base)
  (--map (list row it) base))

(defun day03/-create-halo (row n-coord)
  (let ((base (number-sequence (1- (car n-coord)) (cadr n-coord)))
        (halo (advent/table)))
    (--each (list (1- row) row (1+ row))
      (-each (day03/-create-coords it base)
        (lambda (value)
          (advent/put halo value t))))
    halo))

(defun day03/intersects? (n-pos s-pos)
  "Dumb and slow intersection routine"
  (advent/get (day03/-create-halo (car n-pos) (cadr n-pos)) s-pos))

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
    (list current-row (reverse matches))))

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
    (list current-row (reverse matches))))

(defun day03/read-line (row s)
  (let ((current-row+matches (day03/read-numbers row (s-replace "." " " s))))
    (list :row row
          :numbers (cadr current-row+matches)
          :symbols (cadr (day03/read-symbols row (car current-row+matches))))))

(defun day03/read-lines (lines)
  "-> ((0 numbers symbols) (1 numbers symbols) ...)"
  (cadr
   (-reduce-from (lambda (acc value)
                   (let ((row (1+ (car acc)))
                         (data (cadr acc)))
                     (push (day03/read-line row value) data)
                     (list row data)))
                 '(-1 nil)
                 lines)))

(defun day03/part-1 (lines)
  )

(defun day03/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day03)
