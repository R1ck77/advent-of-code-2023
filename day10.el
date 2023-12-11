(require 'dash)
(require 'advent-utils)
(require 's)

;;(setq example (advent/read-grid 10 :example #'day10/-to-symbol))
;;(setq problem (advent/read-grid 10 :problem #'day10/-to-symbol))

(setq day10/neighbors '((-1 0) (1 0) (0 -1) (0 1)))

(setq day10/connections (list :| '((-1 . 0) (1 . 0))
                              :- '((0 . -1) (0 . 1))
                              :L '((-1 . 0) (0 . 1))
                              :J '((-1 . 0) (0 . -1))
                              :7 '((1 . 0) (0 . -1))
                              :F '((1 . 0) (0 . 1))
                              :. '()
                              :S '((-1 . 0) (1 . 0) (0 . -1) (0 . 1))))

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

(defun day10/get-loop-size (grid)
  (advent/table-size
   (plist-get (day10/finish-loop! (day10/create-traversal-state grid)
                                  (day10/find-S grid))
              :visited)))

(defun day10/part-1 (grid)
  (/ (day10/get-loop-size grid) 2))

(defun day10/part-2 (grid)
  (error "Not yet implemented"))

(provide 'day10)
