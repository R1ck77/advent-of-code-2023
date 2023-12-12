(require 'dash)
(require 'advent-utils)
(require 's)

(defun day12/read-digits (s)
  (-map #'string-to-number (s-split "," s)))

(defun day12/s-to-symbol (s)
  (cond
   ((string= "?" s) :x)
   ((string= "#" s) :s)
    ((string= "." s) :empty)))

(defun day12/read-glyphs (s)
  (-map #'day12/s-to-symbol
        (s-split "" s t)))

(defun day12/read-line (line)
  (let ((tokens (s-split " " line)))
    (list :glyphs (day12/read-glyphs (car tokens))
          :digits (day12/read-digits (cadr tokens)))))

(defun day12/read-data (lines)
  (-map #'day12/read-line lines))

(setq example (day12/read-data (advent/read-problem-lines 12 :example)))
(setq problem (day12/read-data (advent/read-problem-lines 12 :problem)))

(defun day12/is-complete? (data)
  (not (-contains? (plist-get data :glyphs) :x)))

(defun day12/take-solved-part (data)
  (--take-while (not (eq it :x))
                (plist-get data :glyphs)))

(defun day12/is-spring? (value)
  (eq value :s))

(defun day12/is-space? (value)
  (eq value :empty))

(defun day12/drop-non-spring (glyphs)
  (--drop-while (not (eq it :s)) glyphs))

(defun day12/take-spring-part (glyphs)
  "Returns the number of removed parts, and the rest of the glyphs"
  (let ((springs (length (-take-while #'day12/is-spring? glyphs))))
    (list springs
          (-drop springs glyphs))))

(defun day12/find-springs (data)
  (let ((rsprings)
        (glyphs (plist-get data :glyphs)))
    (while glyphs
      (let ((space-removed (-drop-while #'day12/is-space? glyphs)))
        (if (eq (car space-removed) :empty)
            (setq glyphs nil)
          (let ((springs-rest (day12/take-spring-part space-removed)))
            (if (eq :empty (caadr springs-rest))
                (setq glyphs nil)
              (setq glyphs (cadr springs-rest))
              (push (car springs-rest rsprings))
                )
            )
          )
        )      
      )
    (reverse rsprings)))

(defun day12/is-compatible? (data)
  (let ((score (day12/find-springs data))
        (digits (plist-get data :digits)))
    ;;; missing check for completeness!
    (unless (> (length score) (length digits))
      (--all? (eq (car it) (cdr it))
              (-zip score digits)))))

(defun day12/get-alternatives (data)
  (cl-assert (not (day12/is-complete? data)))
  (let ((glyphs (plist-get data :glyphs))
        (digits (plist-get data :digits)))
    (-filter #'day12/is-compatible?
             (--map (list :glyphs it
                          :digits digits)
                    (list (-replace-first :x :s glyphs)
                          (-replace-first :x :empty glyphs))))))


(defun day12/part-1 (lines)
  (error "Not yet implemented"))

(defun day12/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day12)
