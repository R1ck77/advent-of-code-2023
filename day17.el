(require 'dash)
(require 'advent-utils)
(require 's)
(require 'avl-tree)

(defconst day17/direction-turns '(((1 . 0) . ((0 . 1) (0 . -1)))
                                  ((-1 . 0) . ((0 . 1) (0 . -1)))
                                  ((0 . 1) . ((1 . 0) (-1 . 0)))
                                  ((0 . -1) . ((1 . 0) (-1 . 0)))))

(defun day17/read-data (lines)
  (advent/lines-to-grid lines #'string-to-number))

(defvar problem (day17/read-data (advent/read-problem-lines 17 :problem)))
(defvar example (day17/read-data (advent/read-problem-lines 17 :example)))

(defstruct day17/crucible "State of the crucible"
           ;; amount of heat dispersed
           heat
           ;; direction the crucible is moving ((1 . 0) down, (-1 . 0) up, (0 . 1) right and, (0 . -1) left)
           dir
           ;; number of tiles moved in a specific direction
           speed
           ;; the position on the grid
           pos)

(defstruct day17/problem "State of the simulation"
           ;; The static grid with the temperature
           grid
           ;; The size of the grid (ok, it's derivate information, but it's a pain to get it everytime)
           size
           ;; The current moves, as day17/crucible
           moves)

;; TODO/FIXME messed up! I'm merging the two solutions
(defun day17/compare-crucibles (a b)
  (< (day17/crucible-heat a)
     (day17/crucible-heat b)))

(defun day17/create-starting-moves ()
  (let ((tree (avl-tree-create #'day17/compare-crucibles)))
    (avl-tree-enter tree (make-day17/crucible :heat 0
                                          :dir '(1 . 0)
                                          :speed 0
                                          :pos '(0 . 0)))
    (avl-tree-enter tree (make-day17/crucible :heat 0
                                          :dir '(0 . 1)
                                          :speed 0
                                          :pos '(0 . 0)))
    tree))

(defun day17/create-problem (grid)
  (make-day17/problem :grid grid
                      :size (advent/get-grid-size grid)
                      :moves (day17/create-starting-moves)))
(defun day17/feasible-directions (crucible)
  "List of directions the crucible could possibly take.

Accounds for the current crucible direction and speed only"
  (let ((dir (day17/crucible-dir crucible)))
   (append (and (< (day17/crucible-speed crucible) 3) dir)
           (cdr (assoc dir day17/direction-turns)))))

(defun day17/new-position (crucible dir)
  (advent/cons+ (day17/crucible-pos crucible)
                crucible-dir))

(defun day17/is-valid-move? (size crucible dir)
  (let ((new-pos (day17/new-position crucible dir)))
    (and (>= (car new-pos) 0)
         (>= (cdr new-pos) 0)
         (< (car new-pos) (car size))
         (< (cdr new-pos) (cdr size)))))

(defun day17/update-crucible (grid crucible dir)
  (let* ((new-pos (day17/new-position crucible dir))
         (new-heat (+ (advent/grid-get grid new-pos)
                      (day17/crucible-heat crucible))))
    (make-day17/crucible :heat new-heat
                         :dir dir
                         :speed (+ (day17/crucible-speed crucible)
                                   (if (equal dir (day17/crucible-dir crucible)) 1 0))
                         :pos new-pos)))

(defun day17/evolve-crucible (grid size crucible)
  (--map (day17/update-crucible grid crucible it)
         (--filter (day17/is-valid-move? size crucible it)
                   (day17/feasible-directions crucible))))

(defun day17/evolve-move! (problem)
  ;; TODO/FIXME check the destructuring-bind thingy
  (let ((grid (day17/problem-grid problem))
        (size (day17/problem-size problem))
        (moves (day17/problem-moves problem)))
    (let ((evolved-crucible (day17/evolve-crucible grid size (avl-tree-stack-pop (avl-tree-stack  moves)))))
      (--each evolved-crucible
        (avl-tree-enter moves it)))))

(defun day17/part-1 (lines)
  (day17/read-data lines)
)

(defun day17/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day17)
