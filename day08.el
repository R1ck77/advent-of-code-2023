(require 'dash)
(require 'advent-utils)
(require 's)

(defconst day08/label "\\([0-9A-Z]+\\)")

(defconst day08/node-regex (format "%s = (%s, %s)"
                                   day08/label
                                   day08/label
                                   day08/label))

(defun day08/to-symbol (string)
  (read (concat ":" string)))

(defun day08/read-node-labels (node)
  (-map #'day08/to-symbol (cdr (s-match day08/node-regex node))))

(defun day08/add-node (nodes node)
  (let ((values (day08/read-node-labels node)))
    (advent/put nodes
                (car values)
                (cons (elt values 1)
                      (elt values 2)))))


(defun day08/read-nodes (lines)
  (let ((nodes (advent/table)))    
    (--each lines (day08/add-node nodes it))
    nodes))

(defun day08/read-instructions (line)
  (-map #'day08/to-symbol (s-split "" line t)))

(defun day08/read-data (lines)
  (list :lr (-cycle (day08/read-instructions (car lines)))
        :nodes (day08/read-nodes (cdr lines))))

(defun day08/dir-func (direction)
  (case direction
    (:L #'car)
    (:R #'cdr)))

(defun day08/next-node (nodes current direction)
  (let ((l+r (advent/get nodes current)))
    (funcall (day08/dir-func direction) l+r)))

(defun day08/nodes (data)
  (plist-get data :nodes))

(defun day08/lr (data)
  (plist-get data :lr))


(defun day08/to-zzz! (data)
  (let ((current :AAA)
        (lr (day08/lr data))
        (nodes (day08/nodes data))
        (count 1))
    (while (not (eq :ZZZ (setq current (day08/next-node nodes current (pop lr)))))
      (setq count (1+ count)))
    count))

(defun day08/part-1 (lines)
  (day08/to-zzz! (day08/read-data lines)))

(defun day08/-key-ends-with (key letter)
  (s-ends-with? letter (symbol-name key)))

(defun day08/is-starting-point? (key)
  (day08/-key-ends-with key "A"))

(defun day08/is-ending-point? (key)
  (day08/-key-ends-with key "Z"))

(defun day08/get-starting-points (data)
  (-filter #'day08/is-starting-point?
           (advent/-map-hash (day08/nodes data) it-key)))

(defun day08/find-steps-for-2-ends (start data)
  "Get to the end 2 times, return the steps required"
  (let ((lr (day08/lr data))
        (nodes (day08/nodes data))
        (current start)
        (step 0)
        (ends nil))
    (while (< (length ends) 2)
      (setq step (1+ step))
      (setq current (day08/next-node nodes current (pop lr)))
      (when (day08/is-ending-point? current)
        (push (cons current step) ends)
        (sit-for 0)))
    (reverse ends)))

(defun day08/find-period (start data)
  (let* ((times (day08/find-steps-for-2-ends start data))
         (candidate (car times)))
    ;; Ensure that my hypotesis that there are no multiple ends
    (assert (eq (car candidate) (car (cadr times))))
    (cdr candidate)))

(defun day08/find-periods (data)
  (--map (day08/find-period it data) (day08/get-starting-points data)))

(defun day08/find-minimum-period (periods)
  (apply #'cl-lcm periods))

(defun day08/part-2 (lines)
  (day08/find-minimum-period
   (day08/find-periods
    (day08/read-data lines))))

(provide 'day08)
