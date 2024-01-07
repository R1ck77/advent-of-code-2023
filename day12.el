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

(defun day12/is-complete? (data)
  (not (s-contains? "?" (plist-get data :s))))

(defun day12/empty-to-nil (s)
  (unless (s-blank? s) s))

(defun day12/drop-last-spring (s)
  (s-reverse (cadr (s-match "#*\\(.*\\)" (s-reverse s)))))

(defun day12/get-useful-part (s)
  ;;; TODO/FIXME investigate plet better
  (pcase-let ((`(ignore ,start ,rest) (s-match "\\([^?]*\\)\\(.*\\)" s)))
    (unless (s-blank? start)
      (if (s-starts-with? "?" rest)
          (day12/empty-to-nil (day12/drop-last-spring start))
        start))))

(defun day12/find-springs (data)
  (if-let ((solved-part (day12/get-useful-part (plist-get data :s))))
      (-map #'length
            (s-split "[.]+" solved-part t))))

(defun day12/is-compatible? (data)
  (s-match (day12/create-regex (plist-get data :digits))
           (plist-get data :s)))

(defun day12/experiment (s)
  (s-split "[.]" s t))

(defun day12/replace-at (s index new-value)
  (let ((new-string (copy-sequence s)))
    (aset new-string index (string-to-char new-value))
    new-string))

(defun day12/random-? (s)
  (let ((q-marks (--find-indices (string= "?" it) (s-split "" s t))) )
    (elt q-marks (random (length q-marks)))))

(defun day12/half-? (s)
  (let ((q-marks (--find-indices (string= "?" it) (s-split "" s t))) )
    (elt q-marks (/ (length q-marks) 2)))
  )

(defun day12/first-? (s)
  (s-index-of "?" s))

(defun day12/break-largest-? (s)
  (let* ((block-size (apply #'max (-map #'length (s-split "[.#]" s t))))
         (largest-block (s-repeat block-size "?")))
    (+ (s-index-of largest-block s) (/ block-size 2))))

(defun day12/select-next-?-to-replace (s)
  (day12/break-largest-? s))

(defun day12/get-alternatives (data)
  (cl-assert (not (day12/is-complete? data)))
  (let ((s (plist-get data :s))
        (digits (plist-get data :digits)))
    (let ((selected-? (day12/select-next-?-to-replace s)))
      (cl-assert selected-?)      
      (-filter #'day12/is-compatible?
               (-map #'day12/cache-missing-dots
                     (--map (list :s it
                                  :digits digits)
                            (list (day12/replace-at s selected-? "#")
                                  (day12/replace-at s selected-? "."))))))))

(defun day12/find-combinations (data)
  (if (day12/is-complete? data)
      (-filter #'day12/is-compatible? (list data))
    (let ((new-combinations (day12/get-alternatives data)))
      (apply #'append (-map #'day12/find-combinations new-combinations)))))

(defun day12/count-combinations (data)
  (length (day12/find-combinations data)))

(defun day12/can-be-divided? (s)  
  (--any? (s-match "^[#]+$" it) (s-split "[.]" s t)))

(defun day12/split-digits (index digits)
  (list (-take index digits)
        (-drop (1+ index) digits)))

(defun day12/find-digit-combinations (value digits)
  "Returns a list of couples of split digits.

nil is returned if splitting is impossible"
  (if-let ((indices (-elem-indices value digits )))
      (--map (day12/split-digits it digits) indices)))

(defun day12/split-s (s)
  (let* ((tokens (s-split "[.]" s t))
         (first-index (--find-index (s-match "^[#]+$" it) tokens))
         (first-string (apply #'concat (-interpose "." (-take first-index tokens))))
         (second-string (apply #'concat (-interpose "." (-drop (1+ first-index) tokens )))))
    (list (length (elt tokens first-index))
          first-string
          second-string)))

(defun day12/build-subdata (s1-s2 d1-d2)
  (list (day12/cache-missing-dots (list :s (car s1-s2)
                                        :digits (car d1-d2)))
        (day12/cache-missing-dots (list :s (cadr s1-s2)
                                        :digits (cadr d1-d2)))))

(defun day12/sort (data1-data2)
  (if (< (apply #'+ (plist-get (car data1-data2) :digits))
         (apply #'+ (plist-get (cadr data1-data2) :digits)))
      data1-data2
    (reverse data1-data2)))

(defun day12/evaluate-pair (data1-data2)
  (let ((data1-data2 (day12/sort data1-data2)))
   (let ((result1 (day12/count-combinations-recursively (car data1-data2))))
     (if (zerop result1)
         0
       (* result1 (day12/count-combinations-recursively (cadr data1-data2)))))))

(defun day12/compute-subproblems (data)
  (let* ((digit-part1-part2 (day12/split-s (plist-get data :s)))
         (part1-part2 (rest digit-part1-part2))
         (digit-combinations (day12/find-digit-combinations (car digit-part1-part2) (plist-get data :digits))))
    (if (not digit-combinations)
        ;; no splitting yields a valid result
        0
      ;; try to evaluate the sub-combinations
      (let ((results (--filter (not (zerop it))
                               (-map #'day12/evaluate-pair
                                     (--map (day12/build-subdata part1-part2 it) digit-combinations)))))
        (apply #'+ results)))))

(defun day12/is-obviously-incompatible? (data)
  (let* ((s (plist-get data :s))
         (digits (plist-get data :digits)))
    (unless (< (length (s-split "[.]" s t)) (length digits))
      (let ((no-spaces (s-replace "." "" s))
             (total (apply #'+ digits)))
        (or (< (length no-spaces) total)
            (> (length (s-replace "?" "" no-spaces)) total))))))

(defun day12/dividi-et-imperat (data)
  (if (day12/is-obviously-incompatible? data)
      0
    (if (day12/is-complete? data)
        (if (day12/is-compatible? data) 1 0)
      (if (< (length (plist-get data :s)) regexp-processing-range)
          (day12/count-combinations data)
        (let ((s (plist-get data :s))
              (digits (plist-get data :digits)))
          (if (day12/can-be-divided? s)
              (day12/compute-subproblems data)
            (apply #'+ (-map #'day12/count-combinations-recursively (day12/get-alternatives data)))))))))

;;; TODO/FIXME remove: the caching doesn't give anything
(setq db (advent/table))
(setq max-lisp-eval-depth 10000)
(setq do-cache t)
(setq from-range 10)
(setq to-range 200)
(setq regexp-processing-range 5)

(defun day12/in-range (x a b)
  (and (>= x a)
       (<= x b)))

(defun day12/count-combinations-recursively (data)
  (if do-cache
      (if-let ((big (day12/in-range (length (plist-get data :s)) from-range to-range))
               (result (advent/get db data)))
          (progn
            ;; (message "*")
            result)
        (let ((computed (day12/dividi-et-imperat data)))
          (advent/put db data computed)
          computed))
    (day12/dividi-et-imperat data)))

(defun day12/sum-all-combinations (data)
  (let ((sum 0))
    (--each data
;      (message "Processing %s %s" (plist-get it :s) (plist-get it :digits))
      (setq sum (+ sum (day12/count-combinations-recursively it))))
    sum))

(defun day12/count-missing-dashes (data)
  (-  (apply #'+ (plist-get data :digits))
      (s-count-matches "[#]" (plist-get data :s))))

(defun day12/count-missing-dots (data)
  (- (s-count-matches "[?]" (plist-get data :s))
     (day12/count-missing-dashes data)))

(defun day12/cache-missing-dots (data)
  (append data (list :dots (day12/count-missing-dots data))))


(defun day12/unfold (data)
  (let ((digits (apply #'append (-repeat 5 (plist-get data :digits)))))
    (list :s (apply #'concat (-interpose "?" (-repeat 5 (plist-get data :s))))
          :digits digits)))

(defun day12/?-indices (s)
  (-map #'car
        (--filter (= (cdr it) ??)
             (--map-indexed (cons it-index it)
                            (-map #'identity s)))))

(defun day12/add-dot-to-data (data index)
  (list :s (day12/replace-at (plist-get data :s) index ".")
        :dots (1- (plist-get data :dots))
        :digits (plist-get data :digits)))

(defun day12/generate-combinations (data)
  (--map (day12/add-dot-to-data data it)
         (day12/?-indices (plist-get data :s))))

(defun day12/resolve-incognita (data)
  "If there are 0 available dots, replace all ? with #"
  (let ((missing-dots (plist-get data :dots)))
    (if (zerop missing-dots)
        (list :s (s-replace "?" "#" (plist-get data :s))
              :digits (plist-get data :digits)
              :dots 0)
      data)))

(defun day12/get-solution-score (data)
  (cl-assert (zerop (plist-get data :dots)))
  (let ((s (plist-get data :s))
        (digits (plist-get data :digits)))
    (if (equal (-map #'length (s-split "[.]+" s t))
               digits)
        1
      0)))

(defun day12/new-count-combinations (unchecked-data &optional cache)
  (let ((data (day12/resolve-incognita unchecked-data))
        (cache (or cache (advent/table))))
    (if (advent/get cache (plist-get data :s)) 0
      (advent/put cache (plist-get data :s) t)
      (if (zerop (plist-get data :dots))
          (day12/get-solution-score data)
        (apply #'+ (--map (day12/new-count-combinations it cache)
                          (day12/generate-combinations data)))))))

(defun day12/new-resolve-problem (data-list)
    (apply #'+
         (-map #'day12/new-count-combinations
               (--map (progn
                        (message "Processing %s" it)
                        it)
                data-list))))

(defun day12/part-1 (lines)
  (day12/new-resolve-problem
   (-map #'day12/cache-missing-dots
         (day12/read-data lines))))


(defun day12/part-2 (lines)
  (day12/new-resolve-problem
   (-map #'day12/cache-missing-dots
         (-map #'day12/unfold
               (day12/read-data lines)))))

(setq example (-map #'day12/cache-missing-dots (day12/read-data (advent/read-problem-lines 12 :example))))
(setq problem (-map #'day12/cache-missing-dots (day12/read-data (advent/read-problem-lines 12 :problem))))



(provide 'day12)

