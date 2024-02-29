#lang racket
;; Variables
(define empty-cell 0)
(define occupied-cell 1)
(define destroyed-cell 2)

;; Grid setup
(define GRID (list (vector 0 0 0 0 0 0 0 0)
                   (vector 0 0 0 0 0 0 0 0)
                   (vector 0 0 0 0 0 0 0 0)
                   (vector 0 0 0 0 0 0 0 0)
                   (vector 0 0 0 0 0 0 0 0)
                   (vector 0 0 0 0 0 0 0 0)
                   (vector 0 0 0 0 0 0 0 0)
                   (vector 0 0 0 0 0 0 0 0)))

(define grid-ht (hash
                 "A" (first GRID)
                 "B" (second GRID)
                 "C" (third GRID)
                 "D" (fourth GRID)
                 "E" (fifth GRID)
                 "F" (sixth GRID)
                 "G" (seventh GRID)
                 "H" (eighth GRID)))

;; Grid functions
(define set-state-grid (lambda (y x state)
                         (vector-set! (hash-ref grid-ht y) (- x 1) state)))

;; Structs
(struct ship (length position) #:mutable)

(define carrier (ship 5 (list '() '() '() '() '() )))
(define battleship (ship 4 (list '() '() '() '() )))
(define cruiser (ship 3 (list '() '() '() )))
(define submarine (ship 3 (list '() '() '() )))
(define destroyer (ship 2 (list '() '() )))


