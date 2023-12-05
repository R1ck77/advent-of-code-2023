;; -*- lexical-binding: t -*-
(require 'dash)
(require 'advent-utils)
(require 's)

;;; TODO/FIXME statements order

;;; TODO/FIXME remove
(setq example  (advent/read-blocks-of-lines 5 :example))
(setq problem  (advent/read-blocks-of-lines 5 :problem))

(defun day05/-read-values-line (rule)
  (-map #'string-to-number (-map #'s-trim (s-split " " rule t))))

(defun day05/-read-values (rules)
  (-map #'day05/-read-values-line rules))

(defun day05/-read-label (line)
  (read (concat ":" (car (s-split " " line)))))

(defun day05/-read-map (line-block)
  (list (day05/-read-label (car line-block))
        (day05/-read-values (cdr line-block))))

(defun day05/-read-seeds (line)
  (assert (s-starts-with? "seeds: " line))
  (let ((raw-numbers (s-split " " (cadr (s-split ": " line)) t)))
   (list :seeds (-map #'string-to-number raw-numbers))))

(defun day05/read-rules (line-blocks)
  (assert (= (length line-blocks) 8))
  (append (day05/-read-seeds (caar line-blocks))
          (day05/-read-map (elt line-blocks 1))
          (day05/-read-map (elt line-blocks 2))
          (day05/-read-map (elt line-blocks 3))
          (day05/-read-map (elt line-blocks 4))
          (day05/-read-map (elt line-blocks 5))
          (day05/-read-map (elt line-blocks 6))
          (day05/-read-map (elt line-blocks 7))))

(defun day05/-rule-to-function (dest-src-len)
  (let* ((length (elt dest-src-len 2))
         (dest-start (elt dest-src-len 0))
         (src-start (elt dest-src-len 1))
         (src-end (+ src-start length))
         (shift (- dest-start src-start)))
    (lambda (number)
      (and (>= number src-start)
           (<= number src-end)
           (+ number shift)))))

(defun day05/-rules-to-function (rules)
  "This is little thing is something I'm actually proud of… 😎"
  (let ((rules-f (-map #'day05/-rule-to-function rules)))
    (lambda (number)
      (or (--reduce-from (or acc (funcall it number)) nil rules-f)
          number))))

(defun day05/-create-functional-rules (rules-plist)
  (assert (eq :seeds (car rules-plist)))
  (let ((seeds (plist-get rules-plist :seeds))        
        (raw-rules (cddr rules-plist)))
    (list :seeds seeds
          :rules (-map #'day05/-rules-to-function
                       (-map #'cadr (-partition 2 raw-rules))))))

(defun day05/-convert-values-with-rule (values rule)
  (--map (funcall rule it) values))

(defun day05/-map-seeds (functional-rules)
  (--reduce-from (day05/-convert-values-with-rule acc it)
                 (plist-get functional-rules :seeds) 
                 (plist-get functional-rules :rules)))


(defun day05/part-1 (line-blocks)
  (apply #'min (day05/-map-seeds
                (day05/-create-functional-rules
                 (day05/read-rules line-blocks)))))

(defun day05/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day05)
