(require 'dash)
(require 's)
(setq load-path (cons "." load-path))

(defun last-test ()
  (car
   (sort (--filter (s-suffix? "-test.el" it)
                   (directory-files "."))
         #'string>)))

(defun run-test-file (test-file)
    (with-temp-buffer    
    (insert-file test-file)
    (eval-buffer)
    (buttercup-run)))

(defun run-last-test ()
  (run-test-file (last-test)))

(defun run-test (number)
  (run-test-file (format "day%2.2d-test.el" number)))
