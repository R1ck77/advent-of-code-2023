(require 'dash)
(require 'advent-utils)
(require 's)

(defun day04/-read-numbers-list (token)
  (-map #'string-to-number (s-split " " token t)))

(defun day04/-read-given-numbers (token)
  (day04/-read-numbers-list token))

(defun day04/-read-winning-numbers (token)
  (day04/-read-numbers-list token))

(defun day04/-read-card-number (token)
  (string-to-number
   (elt (s-match "Card +\\([0-9]+\\)" token) 1)))

(defun day04/-split-tokens (line)
  (-map #'s-trim (s-split "|" (s-replace ":" "|" line))))

(defun day04/read-data (lines)
  (--map (list
          (day04/-read-card-number (elt it 0))
          (day04/-read-winning-numbers (elt it 1))
          (day04/-read-given-numbers (elt it 2)))
         (-map #'day04/-split-tokens lines)))

(defun day04/part-1 (lines)
  (error "Not yet implemented"))

(defun day04/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day04)
