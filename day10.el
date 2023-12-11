(require 'dash)
(require 'advent-utils)
(require 's)

(setq example (advent/read-grid 10 :example #'day10/-to-symbol 5))
;;(setq problem (advent/read-grid 10 :problem #'day10/-to-symbol))

(defconst day10/connections (list :| '((-1 . 0) (1 . 0))
                              :- '((0 . -1) (0 . 1))
                              :L '((-1 . 0) (0 . 1))
                              :J '((-1 . 0) (0 . -1))
                              :7 '((1 . 0) (0 . -1))
                              :F '((1 . 0) (0 . 1))
                              :. '()
                              :S '((-1 . 0) (1 . 0) (0 . -1) (0 . 1))))

(defconst day10/pipes '(:| :- :L :J :7 :F))

;;; Assumes that I'm in the lower left quadrant of a grid cell
(defconst day10/border-score (list :| '(:h 1 :v 0)
                                   :- '(:h 0 :v 1)                                  
                                   :L '(:h 0 :v 0)
                                   :J '(:h 0 :v 1)
                                   :7 '(:h 1 :v 1)
                                   :F '(:h 1 :v 0)))

(defmacro day10/with-state (state &rest body)
  (declare (indent 1))
  `(let ((grid (plist-get ,state :grid))
         (visited (plist-get ,state :visited)))
     ,@body))

(defun day10/find-S (grid)
  (let ((coord))
    (advent/-each-grid grid
      (if (eq it-value :S)
          (setq coord it-coord)))    
    coord))

(defun day10/+cc (a b)
  (cons (+ (car a)
           (car b))
        (+ (cdr a)
           (cdr b))))

;; TODO/FIXME slow. Cache, probably
(defun day10/valid-coord? (grid coord)
  (let ((size (advent/get-grid-size grid))
        (row (car coord))
        (column (cdr coord)))
    (and (>= row 0)
         (>= column 0)
         (< row (car size))
         (< column (cdr size)))))

(defun day10/get-potential-pipe-neighbors (grid pos)
  (--map (day10/+cc it pos)
         (plist-get day10/connections
                    (advent/grid-get grid pos))))


(defun day10/get-pipe-neighbors (grid pos)
  (--filter (day10/valid-coord? grid it)
            (day10/get-potential-pipe-neighbors grid pos)))

(defun day10/create-traversal-state (grid)
  (let* ((size (advent/get-grid-size grid))
         (visited (advent/table)))
    (advent/put visited (day10/find-S grid) 0)
    (list :grid grid
          :visited visited)))

(defun day10/is-visited? (state coord)
  (advent/get (plist-get state :visited) coord))

(defun day10/connected-to-me? (grid me other)
  (-contains? (day10/get-potential-pipe-neighbors grid other) me))

(defun day10/get-viable-neighbors (state coord)
  (let ((grid (plist-get state :grid)))
    (--filter (day10/connected-to-me? grid coord it)
           (--filter (not (day10/is-visited? state it))
                     (day10/get-pipe-neighbors grid coord)))))

(defun day10/finish-loop! (state coord)
  (let* ((candidates nil)
        (visited (plist-get state :visited))
        (current-score (advent/get visited coord)))
    (while (setq candidates (day10/get-viable-neighbors state coord))
      (setq coord (car candidates))
      (setq current-score (1+ current-score))
      (advent/put visited coord current-score))
    state))

(defun day10/get-state-for-finished-loop (grid)
  (day10/finish-loop! (day10/create-traversal-state grid)
                      (day10/find-S grid)))

(defun day10/get-loop-size (grid)
  (advent/table-size
   (plist-get (day10/get-state-for-finished-loop grid) :visited)))

(defun day10/part-1 (grid)
  (/ (day10/get-loop-size grid) 2))

(defun day10/find-vertical-path (from to)
  (let ((start-row (car from))
        (end-row (car to))
        (column (cdr from)))
    (cond
     ((= start-row end-row) (list from))
     ((> start-row end-row) (--map (cons it column) (reverse (number-sequence end-row start-row))))
     ((< start-row end-row) (--map (cons it column) (number-sequence start-row end-row))))))

(defun day10/find-horizontal-path (from to)
  (let ((start-column (cdr from))
        (end-column (cdr to))
        (row (car from)))
    (cond
     ((= start-column end-column) (list from))
     ((> start-column end-column) (--map (cons row it) (reverse (number-sequence end-column start-column))))
     ((< start-column end-column) (--map (cons row it) (number-sequence start-column end-column))))))

(defun day10/find-path (from to)
  (let* ((vertical-path (day10/find-vertical-path from to))
         (horizontal-path (day10/find-horizontal-path (car (last vertical-path)) to)))
    (append vertical-path (rest horizontal-path))))

(defun day10/get-score-for-border-crossing (pipe direction)
  (plist-get (plist-get day10/border-score pipe) direction))

(defun day10/crossing-score (state from to)
  (day10/with-state state
    (if (not (numberp (advent/get visited from)))
        0
      (day10/get-score-for-border-crossing (advent/grid-get grid from)
                                           (if (= (car from) (car to)) :h :v)))))

(defun day10/compute-crossings (state path)
  (assert (not (numberp (advent/get (plist-get state :visited) (car path)))))
  (--map (day10/crossing-score state (car it) (cadr it))
         (-partition-in-steps 2 1 path)))

(defun day10/same-side? (state a b)
  "Used on two non path cells to see if they are on the same side" 
  (evenp (apply #'+ (day10/compute-crossings state (day10/find-path a b)))))

(defun day10/find-unvisited-border-tile (state)
  "Find any tile on the border that doesn't belong to the path.

Ths may actually fail for some input, if the path crosses all
tiles around the grid.

In that case, the contingency plan may be to either extend the grid by 1 tile."
  (let ((unvisited-border-cell)
        (grid (plist-get state :grid))
        (visited (plist-get state :visited)))
    (advent/-each-grid grid
      (unless unvisited-border-cell
        (if (advent/get visited it-coord)
            (setq unvisited-border-cell it-coord))))
    (if (not unvisited-border-cell)
      (error "Unaccounted condition: extend the grid by one row or column to fix the problem"))
    unvisited-border-cell))

(defun day10/count-external (state)
  (let ((reference-coord (day10/find-unvisited-border-tile state))
        (visited (plist-get state :visited))
        (grid (plist-get state :grid))
        (count 0))
    (advent/-each-grid grid
      (unless (numberp (advent/get visited it-coord))
        (if (day10/same-side? state it-coord reference-coord)
            (setq count (1+ count)))))
    count))

(defun day10/count-internal (state)
  (let ((grid-size (advent/get-grid-size (plist-get state :grid))))
    (- (* (car grid-size) (cdr grid-size))
       (advent/table-size (plist-get state :visited))
       (day10/count-external state))))

(defun day10/find-S-legs (state)
  (day10/with-state state
    (let ((s (day10/find-S grid))
          (path (--filter #'numberp (advent/-map-hash visited (list it-value it-key)))))
      (setq x path)
      (let ((previous (cadr (assoc (1- (length path)) path)))
            (next (cadr (assoc 1 path))))
        (list previous s next)))))

(defun day10/is-valid-replacement? (grid prev-s-next replacement)
  (let ((prev (elt prev-s-next 0))
        (s (elt prev-s-next 1))
        (next (elt prev-s-next 2)))
    (let ((test-grid (advent/copy-grid grid)))
      (advent/grid-set! test-grid s replacement)
      (let ((s-neighbors (day10/get-potential-pipe-neighbors test-grid s)))
        (and (-contains? s-neighbors prev)
             (-contains? s-neighbors next))))))


(defun day10/find-S-replacement (state)
  (let* ((prev-s-next (day10/find-S-legs state)))
    (day10/with-state state
      (car
       (--filter (day10/is-valid-replacement? grid prev-s-next it) 
                 day10/pipes)))))


(defun day10/replace-S-with-pipe (state)
  (day10/with-state state    
    (let ((s (day10/find-S grid))
          (new-grid (advent/copy-grid grid)))
      (advent/grid-set! new-grid s (day10/find-S-replacement state))
      (list :grid new-grid
            :visited visited))))

(defun day10/part-2 (grid)
  (day10/count-internal
   (day10/replace-S-with-pipe
    (day10/get-state-for-finished-loop grid))))

(provide 'day10)
