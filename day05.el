;; -*- lexical-binding: t -*-
(require 'dash)
(require 'advent-utils)
(require 's)

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

(defun day05/-raw-intersection (range-a range-b)
  "Ranges are expressed as (start . len).

Returns a list: (overlap extra-a-list) where overlap is the part
of a that is in b, and extra is a list of ranges not in b"
  (let ((a-start (car range-a))
        (a-end (+ (car range-a) (cdr range-a)))
        (b-start (car range-b))
        (b-end (+ (car range-b) (cdr range-b))))
    (cond
     ;; a completely included in b
     ((and (>= a-start b-start)
           (<= a-end b-end))
      (list range-a nil))
     ;; a disjointed
     ((or (> a-start b-end)
          (< a-end b-start))
      (list nil (list range-a)))
     ;; b completely in a
     ((and (> b-start a-start)
           (< b-end a-end))
      (list range-b
            (list (cons a-start (- b-start a-start))
                  (cons (1+ b-end) (- a-end b-end)))))
     ;; start of a included in b
     ((>= a-start b-start)
      (let ((overlap (- b-end a-start)))
        (list (cons a-start overlap)
              (list (cons b-end (- (cdr range-a) overlap))))))
     ;; end of a included in b
     ((<= a-end b-end)
      (let ((overlap (- a-end b-start)))
        (list (cons b-start overlap)
              (list (cons a-start (- (cdr range-a) overlap))))))
     (t (error (format "Unexpected condition %s vs %s" range-a range-b)))))
  )

(defun day05/-intersection (range-a range-b)
  "range-a is the seed range, range-b is the rule"
  (let ((intersection (day05/-raw-intersection range-a range-b)))
    ;; Ensure that the intersection doesn't lose numbers along the way
    (list :overlap (car intersection)
          :extra (cadr intersection))))

(defun day05/-shift-range (shift range)
  (if range
   (cons (+ shift (car range))
         (cdr range))))

(defun day05/-rule-to-range-function (dest-src-len)
  (let* ((length (elt dest-src-len 2))
         (dest-start (elt dest-src-len 0))
         (src-start (elt dest-src-len 1))
         (shift (- dest-start src-start)))
    (lambda (seed-range)
      (let ((intersection (day05/-intersection seed-range
                                                (cons src-start length))))
        (list :converted (day05/-shift-range shift (plist-get intersection :overlap))
              
              :passed (plist-get intersection :extra))))))

(defun day05/-filter-seed-ranges (seeds-ranges rule-f)
  (let ((passed (plist-get seeds-ranges :passed))
        (newly-passed)
        (all-converted (plist-get seeds-ranges :converted-list)))
    (let ((after-single-rule (-map (lambda (seed-range)                   
                                     (funcall rule-f seed-range))
                                   passed)))      
      (-each after-single-rule
        (lambda (rule-result)
          (setq newly-passed (append newly-passed (plist-get rule-result :passed)))
          (if-let ((new-converted (plist-get rule-result :converted)))
              (push new-converted  all-converted))))
      (list :passed newly-passed
            :converted-list all-converted))))

(defun day05/-rules-to-range-function (rules)
  (let ((rules-f (-map #'day05/-rule-to-range-function rules)))
    (lambda (seeds-range)
      (let ((conversion-result (-reduce-from
                                #'day05/-filter-seed-ranges
                                (list :passed seeds-range :converted-list nil)
                                rules-f)))
        (append (plist-get conversion-result :passed)
                (plist-get conversion-result :converted-list))))))

(defun day05/-create-seeds-one-ranges (raw-ranges)
  (--map (cons it 1) raw-ranges))

(defun day05/-create-one-range-functional-rules (rules-plist)
    (assert (eq :seeds (car rules-plist)))
  (let ((seeds (day05/-create-seeds-one-ranges
                (plist-get rules-plist :seeds)))        
        (raw-rules (cddr rules-plist)))
    (list :seeds seeds
          :rules (-map #'day05/-rules-to-range-function
                       (-map #'cadr (-partition 2 raw-rules))))))

(defun day05/part-1 (line-blocks)
    (apply #'min (-map #'car
                      (day05/-map-seed-ranges
                       (day05/-create-one-range-functional-rules
                        (day05/read-rules line-blocks))))))


(defun day05/-create-seeds-ranges (raw-ranges)
  (--map (cons (car it) (cadr it))
         (-partition 2 raw-ranges)))

(defun day05/-create-range-functional-rules (rules-plist)
  (assert (eq :seeds (car rules-plist)))
  (let ((seeds (day05/-create-seeds-ranges
                (plist-get rules-plist :seeds)))        
        (raw-rules (cddr rules-plist)))
    (list :seeds seeds
          :rules (-map #'day05/-rules-to-range-function
                       (-map #'cadr (-partition 2 raw-rules))))))

(defun day05/-map-seed-ranges (functional-rules)
  (let ((seeds (plist-get functional-rules :seeds))
        (rules (plist-get functional-rules :rules)))
    (--reduce-from (funcall it acc) seeds rules)))

(defun day05/part-2 (line-blocks)
  (apply #'min (-map #'car
                      (day05/-map-seed-ranges
                       (day05/-create-range-functional-rules
                        (day05/read-rules line-blocks))))))



(provide 'day05)
