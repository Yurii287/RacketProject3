#lang racket
(require racket/trace)

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

;; Ship setup
(struct ship (length position) #:mutable)

(define carrier (ship 5 (list '() '() '() '() '() )))
(define battleship (ship 4 (list '() '() '() '() )))
(define cruiser (ship 3 (list '() '() '() )))
(define submarine (ship 3 (list '() '() '() )))
(define destroyer (ship 2 (list '() '() )))

;; Functions
(define grid-list (hash->list grid-ht #t))

(define set-state-grid (lambda (co-ord state)
                         (let ([y (first co-ord)]
                               [x (second co-ord)])
                           (vector-set! (hash-ref grid-ht y) (- x 1) state))))

(define get-state-grid (lambda (co-ord)
                         (let ([y (first co-ord)]
                               [x (second co-ord)])
                         (vector-ref (hash-ref grid-ht y) (- x 1)))))

(define set-ship-position1 (lambda (ship-name y x lst) ;only works for east placement
                            (cond
                              ([equal? (length lst) (ship-length ship-name)] (set-ship-position! ship-name (reverse lst)))
                              (else (set-ship-position1 ship-name y (+ x 1) (cons (list y x) lst)))
                              )
                            )
  )
