#lang racket

(provide (all-defined-out))
(define P1-GRID (for*/vector ([i 10] [j 10]) (vector i j)))
(define grid-flatten2 (lambda (grid) (flatten (for/list ([i (build-string 10 (lambda (i) (integer->char (+ i 97)) ))]) (vector->list (get-row-whole (string (char-upcase i)) grid))))))
;; Variables
(define empty-cell 0)
(define occupied-cell 1)
(define destroyed-cell 2)

(define square-size 40)

(define active-player " ")
(set! active-player "Player 1")

; Grid setup
(define P1-GRID (for/vector ([i 10]) (make-vector 10)))

(define P2-GRID (for/vector ([i 10]) (make-vector 10)))


(define grid-ht-p1 (hash
                 "A" (vector-ref P1-GRID 0)
                 "B" (vector-ref P1-GRID 1)
                 "C" (vector-ref P1-GRID 2)
                 "D" (vector-ref P1-GRID 3)
                 "E" (vector-ref P1-GRID 4)
                 "F" (vector-ref P1-GRID 5)
                 "G" (vector-ref P1-GRID 6)
                 "H" (vector-ref P1-GRID 7)
                 "I" (vector-ref P1-GRID 8)
                 "J" (vector-ref P1-GRID 9)))

(define grid-ht-p2 (hash
                 "A" (vector-ref P2-GRID 0)
                 "B" (vector-ref P2-GRID 1)
                 "C" (vector-ref P2-GRID 2)
                 "D" (vector-ref P2-GRID 3)
                 "E" (vector-ref P2-GRID 4)
                 "F" (vector-ref P2-GRID 5)
                 "G" (vector-ref P2-GRID 6)
                 "H" (vector-ref P2-GRID 7)
                 "I" (vector-ref P2-GRID 8)
                 "J" (vector-ref P2-GRID 9)))



; Ship setup
(struct ship (name length position P1-state P2-state) #:mutable)

(define carrier (ship "carrier" 5 (list '() '() '() '() '() ) 0 0))
(define battleship (ship "battleship" 4 (list '() '() '() '() ) 0 0))
(define cruiser (ship "cruiser" 3 (list '() '() '() ) 0 0))
(define submarine (ship "submarine" 3 (list '() '() '() ) 0 0))
(define destroyer (ship "destroyer" 2 (list '() '() ) 0 0))

(define ship-list (list (ship-name carrier) (ship-name battleship) (ship-name cruiser) (ship-name submarine) (ship-name destroyer)))

(define active-ships-p1 (set))
(define active-ships-p2 (set))

; Functions
(define grid-flatten (lambda (grid) (flatten (for*/list ([i (range 0 10)]) (vector->list (list-ref (vector->list grid) i))))))

(define grid-list (lambda (grid) (hash->list grid #t)))

(define grid-keys (lambda (grid) (hash-keys grid #t)))

(define get-row-whole (lambda (y grid) (hash-ref grid y)))

(define get-row-index (for*/vector ([i (range 0 10)]) (vector i 0)))

(define get-active-grid (lambda (x)
                          (cond
                            ([equal? x "Player 1"] (displayln "Player 1") P1-GRID)
                            ([equal? x "Player 2"] (displayln "Player 2") P2-GRID)
                            )
                          )
  )

(define get-active-ht (lambda (x)
                        (cond
                          ([equal? x "Player 1"] grid-ht-p1)
                          ([equal? x "Player 2"] grid-ht-p2)
                          )
                        )
  )

(define set-state-grid (lambda (co-ord state grid)
                         (let ([y (first co-ord)]
                               [x (second co-ord)])
                           (vector-set! (hash-ref grid y) (- x 1) state))))

(define get-state-grid (lambda (co-ord grid)
                         (let ([y (first co-ord)]
                               [x (second co-ord)])
                         (vector-ref (hash-ref grid y) (- x 1)))))

(define add-active-ships (lambda (ship player-set)
                           (set-add player-set (ship-name ship))))

(define remove-active-ships (lambda (ship player-set)
                              (set-remove player-set (ship-name ship))))

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
                                    (else (set-ship-position-north ship-name (list-ref (grid-keys (get-active-ht active-player)) (+ (index-of (grid-keys (get-active-ht active-player)) y) 1)) x (cons (list y x) lst)))
                                    )
                                  )
  )

(define set-ship-position-west (lambda (ship-name y x lst)
                            (cond
                              ([equal? (length lst) (ship-length ship-name)] (set-ship-position! ship-name (reverse lst)))
                              ([> x 10] "Can not place ship here")
                              (else (set-ship-position-west ship-name y (+ x 1) (cons (list y x) lst)))
                              )
                            )
  )

(define set-ship-position-south (lambda (ship-name y x lst)
                                  (cond
                                    ([equal? (length lst) (ship-length ship-name)] (set-ship-position! ship-name (reverse lst)))
                                    (else (set-ship-position-south ship-name (list-ref (grid-keys (get-active-ht active-player)) (- (index-of (grid-keys (get-active-ht active-player)) y) 1)) x (cons (list y x) lst)))
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
                               (set-state-grid i occupied-cell (get-active-ht active-player)))))

(define draw-ship-to-grid (lambda (ship-name y x direction)
                            (set-ship-position ship-name y x direction)
                            (set-ship-position-grid ship-name)
                            (cond
                              ([equal? active-player "Player 1"] (set-ship-P1-state! ship-name 1) (set! active-ships-p1 (add-active-ships ship-name active-ships-p1)) (set! active-player "Player 2") P1-GRID)
                              ([equal? active-player "Player 2"] (set-ship-P2-state! ship-name 1) (set! active-ships-p2 (add-active-ships ship-name active-ships-p2)) (set! active-player "Player 1") P2-GRID)
                              )
                            )
  )
