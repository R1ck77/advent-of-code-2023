(require 'dash)
(require 'advent-utils)
(require 's)

(defun day17/direction-turns '(((1 . 0) ((0 . 1) (0 . -1)))
                               ((-1 . 0) ((0 . 1) (0 . -1)))
                               ((0 . 1) ((1 . 0) (-1 . 0)))
                               ((0 . -1) ((1 . 0) (-1 . 0)))))

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
           ;; The current moves, as day17/crucible
           moves)

(defun day17/create-problem (grid)
  (make-day17/problem :grid grid
                      :moves (list (make-day17/crucible :heat 0
                                                        :dir '(1 . 0)
                                                        :speed 0
                                                        :pos '(0 . 0))
                                   (make-day17/crucible :heat 0
                                                        :dir '(0 . 1)
                                                        :speed 0
                                                        :pos '(0 . 0)))))
(defun day17/feasible-directions (crucible)
  "List of directions the crucible could possibly take.

Accounds for the current crucible direction and speed only"
  )

(defun day17/evolve-crucible (grid crucible)
  
  
  )

(defun day17/part-1 (lines)
  (day17/read-data lines)
)

(defun day17/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day17)
