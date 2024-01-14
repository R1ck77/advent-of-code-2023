(require 'dash)
(require 'advent-utils)
(require 's)

(defun day15/read-data (lines)
  (s-split "," (car lines) t))

(defun day15/compute-hash (s)
  (--reduce-from (% (* (+ acc it) 17) 256) 0  (string-to-list s)))

(defun day15/compute-total-hash (xs)
  (apply #'+ (-map #'day15/compute-hash xs)))

(defun day15/part-1 (lines)
  (day15/compute-total-hash (day15/read-data lines)))

(defun day15/- (box-content label)
  (--remove (eq (car it) label) box-content))

(defun day15/= (box-content label fl)
  (if (--any? (eq (car it) label) box-content)
      (--map (if (eq (car it) label)
                 (cons label fl)
               it)
             box-content)
    (cons (cons label fl) box-content)))

(defun day15/make-label (s)
  (let ((symbol-name (concat ":" s)))
    (or (intern-soft symbol-name)
        (intern symbol-name))))

(defun day15/read-operation (s)
  (if-let ((result (s-match "\\([a-z]+\\)\\([-=]\\)\\([0-9]*\\)$" s)))
      (list :op (day15/make-label (elt result 2))
            :box (day15/compute-hash (elt result 1))
            :label (day15/make-label (elt result 1))
            :fl (and (not (s-blank? (elt result 3)))
                     (string-to-number (elt result 3))))))

(defun day15/update-box! (boxes index f)
  (let ((old-content (aref boxes index)))
    (aset boxes index (funcall f old-content))))

(defun day15/perform-operation! (boxes op)
  (if (eq (plist-get op :op) :-)
      (day15/update-box! boxes
                        (plist-get op :box)
                        (lambda (box-content)
                          (day15/- box-content
                                   (plist-get op :label))))
    (day15/update-box! boxes
                      (plist-get op :box)
                      (lambda (box-content)
                        (day15/= box-content
                                 (plist-get op :label)
                                 (plist-get op :fl)))))
  boxes)

(defun day15/create-boxes ()
  (make-vector 256 nil))

(defun day15/organize-lenses (xop)
  (let ((boxes (day15/create-boxes)))
    (--each (-map #'day15/read-operation xop)
      (day15/perform-operation! boxes it))
    boxes))

(defun day15/compute-box-focusing-power (index box-content)
  (apply #'+ (--map-indexed (* (1+ index)
                               (1+ it-index)
                               (cdr it))
                            (reverse box-content))))

(defun day15/compute-boxes-focusing-power (boxes)
  (cl-loop for i from 0 upto 255
           sum (day15/compute-box-focusing-power i (aref boxes i))))


(setq example (day15/read-data (advent/read-problem-lines 15 :example)))
(setq problem (day15/read-data (advent/read-problem-lines 15 :problem)))

(defun day15/part-2 (lines)
  (day15/compute-boxes-focusing-power
   (day15/organize-lenses
    (day15/read-data lines))))

(provide 'day15)
