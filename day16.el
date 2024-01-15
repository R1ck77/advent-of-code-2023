(require 'dash)
(require 'advent-utils)
(require 's)

(defun day16/read-cell (s)
  (unless (string= s ".")
    (advent/s->k s)))

(defun day16/read-data (lines)
  (advent/lines-to-grid lines #'day16/read-cell))

(setq example (day16/read-data (advent/read-problem-lines 16 :example)))
(setq problem (day16/read-data (advent/read-problem-lines 16 :problem)))

(defmacro day16/with-state (state &rest forms)
  "Non-hygienic macro to simplify unrolling the state"
  (declare (indent 1)
           (debug (form body)))
  `(let ((layout (plist-get ,state :layout))
         (beams (plist-get ,state :beams))
         (heads (plist-get ,state :heads)))
     ,@forms))

(defmacro day16/with-head (head &rest forms)
  "Non-hygienic macro to simplify head-manipulation"
  (declare (indent 1)
           (debug (form body)))
  `(let ((dir (plist-get ,head :dir))
         (pos (plist-get ,head :pos)))
     ,@forms))

(defun day16/valid-pos? (pos grid-size)  
  (and (>= (car pos) 0)
       (>= (cdr pos) 0)
       (< (car pos) (car grid-size))
       (< (cdr pos) (cdr grid-size))
       pos))

(defun day16/mark-beams! (beams head)
  (day16/with-head head
    (cl-assert (day16/valid-pos? pos (advent/get-grid-size beams)))
    (let ((present (advent/grid-get beams pos)))
      (unless (-contains? present dir)
        (advent/grid-set! beams pos (cons dir present))
        head))))

(defun day16/-next-pos (pos dir grid-size)
  (case dir
    (:r (cons (car pos) (1+ (cdr pos))))
    (:l (cons (car pos) (1- (cdr pos))))
    (:u (cons (1- (car pos)) (cdr pos)))
    (:d (cons (1+ (car pos)) (cdr pos)))))

(defconst day16/dir-changes
  (list :/ (list
            :r '(:u)
            :l '(:d)
            :u '(:r)
            :d '(:l)
            )
        :\\ (list
            :r '(:d)
            :l '(:u)
            :u '(:l)
            :d '(:r))
        :- (list
            :r '(:r)
            :l '(:l)
            :u '(:r :l)
            :d '(:r :l))
        :| (list
            :r '(:u :d)
            :l '(:u :d)
            :u '(:u)
            :d '(:d))))

(defun day16/-next-dirs (dir cell)
  (or (plist-get (plist-get day16/dir-changes cell) dir)
      (list dir)))

(defun day16/-evolve-head (layout head)
  "Returns the new heads, if any from this one"
  (day16/with-head head
    (let ((grid-size (advent/get-grid-size layout)))
     (when-let ((next-pos (day16/-next-pos pos dir grid-size)))
       (when (day16/valid-pos? next-pos grid-size)
         (--map (list :dir it
                      :pos next-pos)
                (day16/-next-dirs dir (advent/grid-get layout next-pos))))))))

(defun day16/-evolve-state! (state)
  (day16/with-state state
    (let ((new-heads (--reduce-from (append (day16/-evolve-head layout it) acc) nil heads))
          (filtered))
      (--each new-heads
        (when (day16/mark-beams! beams it)
          (push it filtered)))
      (list :layout layout
            :beams beams
            :heads filtered))))

(defun day16/is-complete? (state)
  (not (plist-get state :heads)))

(defun day16/next! (state)
  "Make a step, or return the same state if no step has to be performed"
  (if (day16/is-complete? state)
      state
    (day16/-evolve-state! state)))

(defun day16/create-state (layout entry-point)
  (list :layout layout
        :beams (advent/make-grid-like layout)
        :heads (list entry-point)))

(defun day16/simulate-light (layout entry-point)
  (let ((state (day16/create-state layout entry-point))
        (next))
    (while (not (eq (setq next (day16/next! state)) state))
      (setq state next))
    state))

(defun day16/-debug-row-to-string (row)
  (--map (if it "#" ".") row))

(defun day16/debug-to-string (state)
  (apply #'concat
         (apply #'append
          (-interpose '("\n") (-map #'day16/-debug-row-to-string
                                    (advent/grid-to-table
                                     (plist-get state :beams)))))))

(defun day16/count-light (state)
  (let ((lights 0))
   (day16/with-state state
     (advent/-each-grid beams
       (when it-value (setq lights (1+ lights)))))
   lights))

(defun day16/part-1 (lines)
  (day16/count-light
   (day16/simulate-light (day16/read-data lines)
                         '(:dir :r :pos (0 . -1)))))

(defun day16/get-starting-heads (layout)
  (let ((grid-size (advent/get-grid-size layout)))
    (append (--map (list :dir :r :pos (cons it -1)) (number-sequence 0 (1- (car grid-size))) )
            (--map (list :dir :l :pos (cons it (cdr grid-size))) (number-sequence 0 (1- (car grid-size))) )
            (--map (list :dir :d :pos (cons -1 it)) (number-sequence 0 (1- (cdr grid-size))) )
            (--map (list :dir :u :pos (cons (car grid-size) it)) (number-sequence 0 (1- (car grid-size))) ))))

(defun day16/count-best-light-for-entry-points (layout)
  (let ((entry-points (day16/get-starting-heads layout)))
    (apply #'max (--map (day16/count-light
                         (day16/simulate-light layout it))
                        entry-points))))

(defun day16/part-2 (lines)
  (day16/count-best-light-for-entry-points
   (day16/read-data lines)))

(provide 'day16)
