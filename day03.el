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

(defun day03/read-tokens (row current-row regex)
  "Returns the modified row and the list of tokens found"
  (let ((matches)
        (current-match))
    (while (setq current-match (car (s-match regex current-row)))
      (let* ((start (s-index-of current-match current-row))
             (end (+ start (length current-match))))
        (push (list current-match
                    row
                    (list start end))
              matches)
        (setq current-row (concat (substring current-row 0 start)
                                  (s-repeat (- end start) " ")
                                  (substring current-row end)))))
    (list current-row (reverse matches))))

(defun day03/update-tokens (conversion-f row+tokens)
  (list (car row+tokens)
        (--map (funcall conversion-f it)
               (cadr row+tokens))))

(defun day03/read-numbers (row current-row)
  (day03/update-tokens (lambda (token)
                         (cons (string-to-number (car token))
                               (cdr token)))
                       (day03/read-tokens row current-row day03/num-regex)))

(defun day03/read-symbols (row current-row)
  (day03/update-tokens (lambda (token)
                         (list (read (concat ":" (car token)))
                               (cadr token)
                               (car (elt token 2))))
                       (day03/read-tokens row current-row day03/symbol-regex)))

(defun day03/read-line (row s)
  (let ((current-row+matches (day03/read-numbers row (s-replace "." " " s))))
    (list :row row
          :numbers (cadr current-row+matches)
          :symbols (cadr (day03/read-symbols row (car current-row+matches))))))

(defun day03/read-lines (lines)
  "-> ((0 numbers symbols) (1 numbers symbols) ...)"
  (reverse
   (cadr
    (-reduce-from (lambda (acc value)
                    (let ((row (1+ (car acc)))
                          (data (cadr acc)))
                      (push (day03/read-line row value) data)
                      (list row data)))
                  '(-1 nil)
                  lines))))

(defun day03/-pad-data (data)
  (append (list (list :row -1 :numbers nil :symbols nil))
          data
          (list (list :row (length data) :numbers nil :symbols nil))))

(defun day03/-is-number-near-symbols? (number symbols-coordinates)
  (--any? (day03/intersects? (rest number) it)
          symbols-coordinates))

(defun day03/-get-adjacent-numbers (a-x-c)
  (let* ((all-symbols (apply #'append (--map (plist-get it :symbols) a-x-c)))
         (symbol-coordinates (--map (rest it) all-symbols))
         (numbers (plist-get (elt a-x-c 1) :numbers)))
    (-map #'car (--filter (day03/-is-number-near-symbols? it symbol-coordinates) numbers))))

(defun day03/-find-adjacent-numbers (data)
  (apply #'append
   (-map #'day03/-get-adjacent-numbers
         (-partition-in-steps 3 1 (day03/-pad-data data)))))

(defun day03/part-1 (lines)
  (apply #'+ (day03/-find-adjacent-numbers (day03/read-lines lines))))

(defun day03/-is-gear-symbol? (symbol)
  (eq :* (car symbol)))

(defun day03/-get-gear-pivot-coords (a-x-c)
  (-map #'rest (-filter #'day03/-is-gear-symbol? (plist-get (elt a-x-c 1) :symbols))))

(defun day03/-get-numbers-near-pivot (pivot-coord numbers)
  (-map #'car
         (--filter (day03/intersects? (rest it) pivot-coord)
                   numbers)))

(defun day03/-get-gears (a-x-c)
  (let* ((pivot-coords (day03/-get-gear-pivot-coords a-x-c))
         (numbers (apply #'append (--map (plist-get it :numbers) a-x-c))))
    (--filter (= (length it) 2)
              (--map (day03/-get-numbers-near-pivot it numbers) pivot-coords))))

(defun day03/-find-gears (data)
  (apply #'append
         (-map #'day03/-get-gears
               (-partition-in-steps 3 1 (day03/-pad-data data)))))

(defun day03/-combine-gear-ratios (gears)
  (apply #'+ (--map (apply #'* it) gears)))

(defun day03/part-2 (lines)
  (day03/-combine-gear-ratios
   (day03/-find-gears
    (day03/read-lines lines))))

(provide 'day03)
