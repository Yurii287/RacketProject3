#lang racket
(require racket/trace)
;; Variables
(define empty-cell 0)
(define occupied-cell 1)
(define destroyed-cell 2)

(define active-player " ")
(set! active-player "Player 1")

;; CHANGE GRID TO VECTOR OF VECTORS, CREATE 2 GRIDS,

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
(struct ship (length position P1placed P2placed) #:mutable)

(define carrier (ship 5 (list '() '() '() '() '() ) #f #f))
(define battleship (ship 4 (list '() '() '() '() ) #f #f))
(define cruiser (ship 3 (list '() '() '() ) #f #f))
(define submarine (ship 3 (list '() '() '() ) #f #f))
(define destroyer (ship 2 (list '() '() ) #f #f))

;; Functions
(define grid-list (hash->list grid-ht #t))

(define grid-keys (hash-keys grid-ht #t))

(define set-state-grid (lambda (co-ord state)
                         (let ([y (first co-ord)]
                               [x (second co-ord)])
                           (vector-set! (hash-ref grid-ht y) (- x 1) state))))

(define get-state-grid (lambda (co-ord)
                         (let ([y (first co-ord)]
                               [x (second co-ord)])
                         (vector-ref (hash-ref grid-ht y) (- x 1)))))

; Place ship directionals -> combine into expandable function later
(define set-ship-position-east (lambda (ship-name y x lst)
                            (cond
                              ([equal? (length lst) (ship-length ship-name)] (set-ship-position! ship-name (reverse lst)))
                              ([< x 1] "Can not place ship here")
                              (else (set-ship-position-east ship-name y (- x 1) (cons (list y x) lst)))
                              )
                            )
  )

(define set-ship-position-north (lambda (ship-name y x lst)
                                  (cond
                                    ([equal? (length lst) (ship-length ship-name)] (set-ship-position! ship-name (reverse lst)))
                                    (else (set-ship-position-north ship-name (list-ref grid-keys (+ (index-of grid-keys y) 1)) x (cons (list y x) lst)))
                                    )
                                  )
  )

(define set-ship-position-west (lambda (ship-name y x lst)
                            (cond
                              ([equal? (length lst) (ship-length ship-name)] (set-ship-position! ship-name (reverse lst)))
                              ([> x 8] "Can not place ship here")
                              (else (set-ship-position-west ship-name y (+ x 1) (cons (list y x) lst)))
                              )
                            )
  )

(define set-ship-position-south (lambda (ship-name y x lst)
                                  (cond
                                    ([equal? (length lst) (ship-length ship-name)] (set-ship-position! ship-name (reverse lst)))
                                    (else (set-ship-position-south ship-name (list-ref grid-keys (- (index-of grid-keys y) 1)) x (cons (list y x) lst)))
                                    )
                                  )
  )

(define set-ship-position (lambda (ship-name y x direction)
                            (cond
                              ([equal? direction "east"] (set-ship-position-east ship-name y x '()))
                              ([equal? direction "west"] (set-ship-position-west ship-name y x '()))
                              ([equal? direction "north"] (set-ship-position-north ship-name y x '()))
                              ([equal? direction "south"] (set-ship-position-south ship-name y x '()))
                              )
                            )
  )

; Draw to grid
(define set-ship-position-grid (lambda (ship-name)
                             (for ([i (ship-position ship-name)])
                               (set-state-grid i occupied-cell))))

(define draw-ship-to-grid (lambda (ship-name y x direction)
                            (set-ship-position ship-name y x direction)
                            (set-ship-position-grid ship-name)
                            (cond
                              ([equal? active-player "Player 1"] (set-ship-P1placed! ship-name #t))
                              ([equal? active-player "Player 2"] (set-ship-P2placed! ship-name #t)))
                            GRID))

; Player Turns
