;; -*- lexical-binding: t -*-
(require 'dash)
(require 'advent-utils)
(require 's)

(defun day12/read-digits (s)
  (-map #'string-to-number (s-split "," s)))

(defun day12/create-regex (digits)
  (concat "^[.?]*"
          (apply #'concat(-interpose "[.?]+"
                                     (--map (format "[#?]\\{%d\\}" it) digits)))
          "[.?]*$"))

(defun day12/read-line (line)
  (let* ((tokens (s-split " " line))
         (digits (day12/read-digits (cadr tokens))))
    (list :s (car tokens)
          :digits digits)))

(defun day12/read-data (lines)
  (-map #'day12/read-line lines))

(defun day12/split-digits (digits index)
  "Given the index of a digit, split digits to the list that precedes and follows that index.

Return the two lists, before and after"
  (list (-take index digits)
        (-drop (1+ index) digits)))

(defun day12/replace-last-char (s)
  (let ((n (length s)))
   (if (zerop n)
       s
     (cl-assert (not (string= "#" (substring s (1- n)))))
     (concat (substring s 0 (1- n)) "."))))

(defun day12/replace-first-char (s)
  (let ((n (length s)))
    (if (zerop n)
        s
     (cl-assert (not (string= "#" (substring s 0 1))))
     (concat "." (substring s 1)))))

(defun day12/split-s-at-region (s index n)
  "Split the string removing n chars from index to index+n.

Returns the remaining two strings with the delimiters changed"
  (let ((from index)
        (to (+ index n)))
    (list (day12/replace-last-char (substring s 0 from))
          (day12/replace-first-char (substring s to)))))

(defun day12/-valid-boundaries? (s from to)
  (and (or (zerop from)
           (not (string= (substring s (1- from) from) "#")))
       (or (= to (length s))
           (not (string= (substring s to (1+ to)) "#")))))

(defun day12/-valid-content? (s from to)
  (not (s-contains? "." (substring s from to))))

(defun day12/can-split-region-at (s index n)
  (let ((from index)
        (to (+ index n)))
    (and (day12/-valid-content? s from to)
         (day12/-valid-boundaries? s from to))))

(defun day12/-is-empty? (s)
  (s-matches? "^[.]*$" s))

(defun day12/-replace-empty (s)
  (unless (day12/-is-empty? s) s))

(defun day12/-filter-empty-from-pair (strings)
  (let ((first (car strings))
        (second (cadr strings)))
    (list (day12/-replace-empty first)
          (day12/-replace-empty second))))

(defun day12/-all-split-combinations (s n)
  (-map #'day12/-filter-empty-from-pair
        (--map (day12/split-s-at-region s it n)
               (--filter (day12/can-split-region-at s it n)
                         (number-sequence 0 (- (length s) n))))))

(defun day12/-create-sub-solution (pre+post-digits pre+post-s)
  (list (list :s (car pre+post-s) :digits (car pre+post-digits))
        (list :s (cadr pre+post-s) :digits (cadr pre+post-digits))))

(defun day12/-find-subdata (data index)
  "Generage a list of pairs of (potentially incoherent) sub-data solutions

elements are only guaranteed to be valid split, but are not checked against digits"
  (let ((digits (plist-get data :digits))
        (s (plist-get data :s)))
    (let ((n (elt digits index))
          (pre+post-digits (day12/split-digits digits index)))
      (--map (day12/-create-sub-solution pre+post-digits it)
             (day12/-all-split-combinations s n)))))

(defun day12/-string-too-short? (data)
  (let* ((s (or (plist-get data :s) ""))
         (digits (plist-get data :digits))
         (digits-sum (apply #'+ digits)))
    (or (< (length s)
           (+ digits-sum (1- (length digits))))
        (< (s-count-matches "[#?]" s)
           digits-sum))))

(defun day12/-digits-too-short? (data)
  (let ((s (or (plist-get data :s) ""))
        (digits (plist-get data :digits)))
    (> (s-count-matches "[#]" s) (apply #'+ digits))))

(defun day12/-incoherent? (data)
  "Returns truthy if the data is obviously invalid"
  (let ((s (plist-get data :s))
        (digits (plist-get data :digits)))
    (or (and (not s) digits)
        (and (not digits) (and s (s-matches? "[#]" s)))
        ;; The next checks are very slow for the small examples, but work for the longer ones
        (day12/-digits-too-short? data)
        (day12/-string-too-short? data))))

(defun day12/-find-coherent-subdata (data index)
  (--filter (not (or (day12/-incoherent? (car it))
                     (day12/-incoherent? (cadr it))))
            (day12/-find-subdata data index)))

(defun day12/-get-next-index (digits)
  (let* ((max (apply #'max digits))
         (max-indices (-map #'car
                           (--filter (= max (cdr it))
                                     (--map-indexed (cons it-index it) digits)))))
    (elt max-indices (/ (length max-indices) 2))))

(defun day12/-sorted-combine-intervals (pair cache)
  (let ((first-value (day12/-count-combinations (car pair) cache)))
    (if (zerop first-value)
        0
      (* first-value (day12/-count-combinations (cadr pair) cache)))))

(defun day12/-combine-intervals (pair cache)
  (if (< (s-count-matches "[?]" (or (plist-get (car pair) :s) ""))
         (s-count-matches "[?]" (or (plist-get (cadr pair) :s) "")))
      (day12/-sorted-combine-intervals pair cache)
    (day12/-sorted-combine-intervals (reverse pair) cache)))

(defun day12/-checked-count-combinations (data cache)
  (let ((digits (plist-get data :digits))
        (s (plist-get data :s)))
    (let* ((next-digits-index (day12/-get-next-index digits))
           (subpairs (day12/-find-coherent-subdata data next-digits-index)))
      (apply #'+  (--map (day12/-combine-intervals it cache)
                         subpairs)))))

(defun day12/-do-with-cache (f data cache)
  (if-let ((cached-result (advent/get cache data)))
      cached-result
    (let ((new-result (funcall f data cache)))
      (advent/put cache data new-result)
      new-result)))

(defun day12/-uncached-count-combinations (data cache)
  (let ((result (if (day12/-incoherent? data) 0
                  (if (plist-get data :digits)
                      (day12/-checked-count-combinations data cache)
                    1))))
    result))

(defun day12/-count-combinations (data &optional cache)
  (let ((cache (or cache (advent/table))))
    (day12/-do-with-cache #'day12/-uncached-count-combinations data cache)))

(defun day12/count-combinations (data)
  (day12/-count-combinations (list :s (concat "." (plist-get data :s) ".")
                                   :digits (plist-get data :digits))))

(defun day12/resolve-problem (data-list)
  (apply #'+
         (-map #'day12/count-combinations
               data-list)))

(defun day12/part-1 (lines)
  (day12/resolve-problem
   (day12/read-data lines)))

(defun day12/unfold (data)
  (let ((digits (apply #'append (-repeat 5 (plist-get data :digits)))))
    (list :s (apply #'concat (-interpose "?" (-repeat 5 (plist-get data :s))))
          :digits digits)))

(defun day12/part-2 (lines)
  (day12/resolve-problem
   (-map #'day12/unfold
         (day12/read-data lines))))

(setq example (day12/read-data (advent/read-problem-lines 12 :example)))
(setq problem (day12/read-data (advent/read-problem-lines 12 :problem)))

(provide 'day12)

