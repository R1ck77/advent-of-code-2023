;; -*- lexical-binding: t -*-
(require 'cl)
(require 'dash)

;; TODO/FIXME do this
;;;(defmacro advent/cons-let (args sequence &rest body)
;;;  )

(defmacro comment (&rest x)
  "Important (and missed…) enough to warrant a global-like name"
  nil)

(defmacro if-present (arg &rest forms)
  "Executes the forms with arg bound to it if it is not nil.

If arg is nil, returns nil without evaluating anything else"
  (declare (indent 1))
  (let ((tempvar (make-symbol "max")))
    `(if-let ((,tempvar ,arg))
         (let ((it ,tempvar))
           ,@forms))))

(defmacro advent/assert (condition &optional message)
  "Not present for some reason"
  `(unless ,condition (error (or ,message "Assertion failed"))))

(defun advent/v->l (vector)
  (append vector '()))

;;;;;;;;;;;;;;;;;;;
;;; TABLE FUNCTIONS

(defun advent/table ()
  (make-hash-table :test #'equal))

(defun advent/copy-table (table)
  (copy-hash-table table))

(defun advent/put (table key value)
  (puthash key value table)
  table)

(defun advent/delete (table key)
  (remhash key table)
  table)

(defun advent/update (table key f &optional default)
  "Update the value of the table at 'key'.

f accepts two arguments: the key and the old value (or 
default is none is present)"
  (advent/put table
              key
              (funcall f key (advent/get table key default))))

(defmacro advent/-update (table key forms &optional default)
  "Anaphoric form of advent/update.

Values are bound to it-key and it-value"
  (let ((table-sym (make-symbol "table"))
        (key-sym (make-symbol "key"))
        (f-key-sym (make-symbol "f-key"))
        (f-value-sym (make-symbol "f-value")))
    `(let ((,key-sym ,key)
           (,table-sym ,table))
       (advent/update ,table-sym
                      ,key-sym
                      (lambda (,f-key-sym ,f-value-sym) ,@forms)
                      ,default))))

(defun advent/set-from (items &optional value)
  "Create a set of the items in the input list.

If value is specified, it is used as a value for the table key,
otherwise the key is duplicated in the value (which may come in handy)"
  (let ((set (advent/table)))
    (--each items (advent/put set it (or value it)))
    set))

(defun advent/cache (table key value)
  "Like advent/put, but returns the value instead of the table"
  (puthash key value table)
  value)

(defun advent/get (table key &optional default)
  (gethash key table default))

(defun advent/update (table key f &optional default &rest other)
  "Update the table using the result of f that accepts the key and the old value (or default).

\"other\" is appended to the list of arguments of f, if present

Returns the table

WARNING: nil values are not properly supported!"
  (let* ((old-value (advent/get table key))
         (new-value (apply f (cons key (cons (or old-value default) other)))))
    (advent/put table key new-value)
    table))

(defmacro advent/-update (table key form &optional default &rest other)
  "Anaphoric form of advent/update

It binds:
    it-key to the key
    it-value to the value
    other to the remaining arguments"
  `(advent/update ,table
                  ,key
                  (lambda (it-key it-value &rest other) ,form)
                  ,default
                  ,other))

;;; TODO/FIXME mixing of hash and table in the names. Either advent/hash or change these
(defun advent/map-hash (table function)
  "Like maphash, but accumulates the return like -map does"
  (let ((result))
    (maphash (lambda (k v)
               (setq result (cons (funcall function k v) result)))
             table)
    (nreverse result)))

(defmacro advent/-map-hash (table &rest forms)
  "Anaphoric version of advent/map-hash that binds key and value to it-key and it-value"
  (declare (indent 1))
  `(advent/map-hash ,table
                    (lambda (it-key it-value)
                      ,@forms)))

(defun advent/each-hash (table function)
  "Same as (maphash function table)"
  (maphash function table))

(defmacro advent/-each-hash (table &rest forms)
  "Anaphoric for for advent/each-hash that binds key and value to it-key and it-value"
  (declare (indent 1))
  `(advent/each-hash ,table (lambda (it-key it-value) ,@forms)))

(defun advent/debug-print-table (table)
  (advent/-each-hash  table
    (message "%s -> %s" it-key it-value)))

(defun advent/table-size (table)
  "Returns the number of elements in a table"
  (hash-table-count table))

;;;;;;;;;;;;;;;;;;
;;; GRID FUNCTIONS

(defun advent/get-grid-size (grid)
  (cons (length grid)
        (length (aref grid 0))))

(defun advent/create--grid-line (row columns)
  (--map (cons row it) (number-sequence 0 (1- columns))))

(defun advent/create-coordinates (rows columns)
  (-flatten (--map (advent/create--grid-line it columns)
                   (number-sequence 0 (1- rows)))))

(defun advent/iterate (f initial-value n)
  (let ((value initial-value))
    (while (> n 0)
      (setq value (funcall f value))
      (setq n (1- n)))
    value))

(defun advent/compute-input-name (day type &optional part)
  (let ((part-name (or (and part (format "-part%d" part)) "")))
    (format (cond
             ((eq type :example)
              "data/day%d-example%s.txt")
             ((eq type :problem)
              "data/day%d-problem%s.txt")
             (t (error "Unexpected problem type")))
            day part-name)))

(defun advent/read-problem-text (day type &optional part)
  (with-temp-buffer
    (insert-file-contents (advent/compute-input-name day type part))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun advent/read-problem-lines (day type &optional part keep-empty)
  (split-string (advent/read-problem-text day type part) "\n" (not keep-empty)))

(defun advent/read--grid-line (line conversion-f)
  (apply #'vector (-map conversion-f (split-string line "" t))))

(defun advent/lines-to-grid (lines &optional conversion-f)
  (apply #'vector
         (--map (advent/read--grid-line it (or conversion-f #'string-to-number))
                lines)))

(defun advent/read-grid (day type &optional conversion-f part)
  (advent/lines-to-grid (advent/read-problem-lines day type part)
                        conversion-f))

(defun advent/make-grid (n-rows n-columns value)
  (let ((rows (make-vector n-rows nil)))
    (cl-loop for row below n-rows do
          (aset rows row (make-vector n-columns value)))
    rows))

(defun advent/make-grid-like (grid &optional value)
  (let ((rows-columns (advent/get-grid-size grid)))
   (advent/make-grid (car rows-columns) (cdr rows-columns) value)))

(defun advent/copy-grid (grid)
  "Returns a copy of the grid (cells are referenced)"
  (apply #'vector
         (--map (copy-sequence (aref grid it))
                (number-sequence 0 (1- (length grid))))))

(defun advent/debug-str-grid (grid &optional format)
  (let ((result ""))
    (cl-loop for i below (length grid) do
          (let ((current-row (aref grid i)))
            (cl-loop for j below (length current-row) do
                  (setq result (concat result " " (format (or format "%s") (aref current-row j))))))
          (setq result (concat result "\n")))
    result))

(defun advent/update-grid-value! (grid coord f)
  (let ((i (car coord))
        (j (cdr coord)))
    (let* ((old-value (aref (aref grid i) j))
           (new-value (funcall f old-value)))
      (aset (aref grid i) j new-value)
      new-value)))

(defmacro advent/-update-grid-value! (grid coord &rest body)
  "Anaphoric form. Binds 'it' to value"
  (declare (indent 2))
  `(advent/update-grid-value! ,grid ,coord (lambda (it) ,@body)))

(defun advent/update-grid! (grid f)
  "Update in place the value of a grid with f, which receives the current cell value as input"
  (cl-loop for i below (length grid) do
        (cl-loop for j below (length (aref grid 0)) do
              (advent/update-grid-value! grid (cons i j) f)))
  grid)

(defmacro advent/-update-grid! (grid &rest forms)
  "Anaphoric version of advent/update-grid!

The value is binded to 'it'"
  (declare (indent 1))
  `(advent/update-grid! ,grid (lambda (it) ,@forms)))

;; TODO/FIXME some serious bug here. If f contains an "f123 it's bad
(defun advent/each-grid (grid f123)
  (cl-loop for i below (length grid) do
        (cl-loop for j below (length (aref grid 0)) do
              (let ((coord (cons i j)))
                (funcall f123 coord (advent/grid-get grid coord ))))))

(defmacro advent/-each-grid (grid &rest forms)
  "macro version of each grid. Values are bound to it-coord and it-value"
  (declare (indent 1))
  `(advent/each-grid ,grid (lambda (it-coord it-value)
                             ,@forms)))

(defun advent/grid-set! (grid row-column value)
  (aset (aref grid (car row-column)) (cdr row-column) value))

(defun advent/grid-get (grid row-column)
  (aref (aref grid (car row-column)) (cdr row-column)))

(defun advent/transpose (grid)
  "Transposes a grid"
  (let* ((size (advent/get-grid-size grid))
         (new-grid (advent/make-grid  (cdr size) (car size) :undefined)))    
    (-each (number-sequence 0 (1- (car size)))
      (lambda (row)
        (-each (number-sequence 0 (1- (cdr size)))
          (lambda (column)
            (advent/grid-set! new-grid
                              (cons column row)
                              (advent/grid-get grid (cons row column)))))))
    new-grid))

(defun advent/rotate-right (grid)
  (let* ((size (advent/get-grid-size grid))
         (new-grid (advent/make-grid  (cdr size) (car size) :undefined)))    
    (-each (number-sequence 0 (1- (car size)))
      (lambda (row)
        (-each (number-sequence 0 (1- (cdr size)))
          (lambda (column)
            (advent/grid-set! new-grid
                              (cons column (- (car size) row 1))
                              (advent/grid-get grid (cons row column)))))))
    new-grid))

(defun advent/grid-to-table (grid)
  "Convert the grid into a list of lists"
  (--map (advent/v->l it) (advent/v->l grid)))

;;;;;;;;;;;;;;;;;;;
;;; PROBLEM READERS

(defun advent/read-problem-numbers-line (day type)
  (-map #'string-to-number
        (split-string (car (advent/read-problem-lines day type))
                      ","
                      t)))

(defun advent/read-problem-numbers (day type)
  (-map #'string-to-number (advent/read-problem-lines day type)))

(defun advent/read-problem-tokens (day type)
  (--map (split-string it " ") (advent/read-problem-lines day type)))

(defun advent/read-problem-instructions (day type)
  (--map (list (intern (concat ":" (car it)))
               (string-to-number (cadr it)))
         (advent/read-problem-tokens day type)))

(defun advent/read-raw-problem-lines (day type)
  (split-string (advent/read-problem-text day type) "\n"))

(defun advent/add-to-accumulator (acc line)
  (let ((groups (car acc))
        (current-group (cadr acc)))
    (if (/= (length line) 0)
        (list groups (cons line current-group))
      (list (cons (reverse current-group) groups) nil))))

(defun advent/group-lines (lines)
  (let ((accumulated (-reduce-from #'advent/add-to-accumulator
                                   '(() ())
                                   lines)))
    (if (cadr accumulated)
        (reverse (cons (reverse (cadr accumulated)) (car accumulated)))
      (reverse (car accumulated)))))

(defun advent/read-blocks-of-lines (day type)
  (advent/group-lines (advent/read-raw-problem-lines day type)))

(defun advent/block-of-lines-to-numbers (line-blocks)
  (--map (-map #'string-to-number it) line-blocks))

(defun advent/bogus-gradient (start end f)
  "A getto gradient function that bisects through the domain"
  (if (< (- end start) 2)
      (let ((start-value (funcall f start))
            (end-value (funcall f end)))
        (if (> end-value start-value)
            (cons start start-value)
          (cons end end-value)))
    (let ((center (floor (/ (+ end start) 2))))
      (let ((center-val (funcall f center))
            (right-val (funcall f (1+ center))))
        (cond
         ((> right-val center-val)
          (advent/bogus-gradient start center f))
         ((< right-val center-val)
          (advent/bogus-gradient center end f))
         (t (error "This is weird…")))))))


(defun advent/--bisect-step (from to f)
  (unless (funcall f to)
    (error (format "Invalid extreme %s" to)))
  (cond
   ((= from to)
    (list from nil))
   ((funcall f from)
    (list from nil))
   ((= (1+ from) to)
    (list to nil))
   (t (let* ((half-point (/ (+ from to) 2))
             (half-result (funcall f half-point)))
        (if half-result
            (list nil from half-point)
          (list nil half-point to))))))

(defun advent/bisect (from to f)
  "Returns the first value in [from to] where f changes from nil to a truthy value.

It's supposed to work on integers

As expected, if there are multiple changes of 'sign', the algorithm will return a non specified one"
  (let ((current-result ))
    (while (not (car (setq current-result (advent/--bisect-step from to f))))      
      (setq from (elt current-result 1))
      (setq to (elt current-result 2)))
    (car current-result)))




(defmacro advent/loop-grid (grid &rest forms)
  "Non-hygienic macro that bind all coordinates of the grid to 'it'

it is bound to the current row and column"
  (declare (indent 1))
  (let ((rows (make-symbol "rows"))
        (columns (make-symbol "columns"))
        (i (make-symbol "i"))
        (j (make-symbol "j"))
        (it (make-symbol "it")))
    `(let ((,rows (length ,grid))
           (,columns (length (aref ,grid 0))))
       (cl-loop for ,i from 0 below ,rows do
             (cl-loop for ,j from 0 below ,columns do
                   (let ((it (cons ,i ,j)))
                     ,@forms))))))

(defmacro advent/time (&rest forms)
  "Time the forms and return a cons with the time in ms and the result"
  (declare (indent 0))
  (let ((start-time (make-symbol "start-time"))
        (result (make-symbol "result")))
    `(let ((,start-time (float-time))
           (,result))
       (setq ,result (progn ,@forms))
       (list (- (float-time) ,start-time)
             ,result))))

(defvar advent/step nil "internal variable used to keep track of the steps")

;; TODO/FIXME this should be converted into an hygienic macro
(defun advent/reset-steps ()
  (setq advent/steps (cons 0 (float-time))))

(defun advent/step (&optional interval)
  (let ((now (float-time))
        (start (cdr advent/steps))
        (step (1+ (car advent/steps))))
    (setq advent/steps (if (< (- now start) (or interval 1))
                           (cons step start)
                         (print (format "%d steps in %s seconds (%s/s)" step (- now start) (/ step (- now start))) )
                         (redisplay)
                         (cons 0 now)))))

;; Misc
(defun advent/s->k (s)
  (let ((symbol-name (concat ":" s)))
    (or (intern-soft symbol-name)
        (intern symbol-name))))

(defun advent/cache-f (f)
  (lexical-let ((cache (advent/table)))
    (lambda (value)
      (or (advent/get cache value)
          (let ((result (funcall f value)))
            (advent/put cache value result)
            result)))))

(provide 'advent-utils)
