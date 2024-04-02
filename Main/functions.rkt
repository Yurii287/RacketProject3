#lang racket
;; to be fixed
;; cannot place anything in "B" due to hashmap referencing.
;; cannot place anything in "J" for same reason.
;; fix ship-hit-check
;; - (cadaar (all-active-pos)) & (caaar (all-active-pos))


(provide (all-defined-out))
(require racket/trace)

;; Variables
(define empty-cell 0)
(define occupied-cell 1)
(define destroyed-cell 2)

(define square-size 40)

(define active-player null)
(set! active-player "Player 1")

; 0 = place ships
; 1 = destroy ships
; 2 = game over
(define game-state null)
(set! game-state 0)

; Grid setup
(define P1-GRID (for/vector ([i 10]) (make-vector 10)))
(define P1-GRID-COORDS (for*/vector ([i 10] [j 10]) (vector i j)))

(define P2-GRID (for/vector ([i 10]) (make-vector 10)))
(define P2-GRID-COORDS (for*/vector ([i 10] [j 10]) (vector i j)))


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
(struct ship (name length P1-position P2-position P1-state P2-state cumulative) #:mutable)

(define carrier (ship "carrier" 5 (list '() '() '() '() '() ) (list '() '() '() '() '() ) 0 0 (* 40 5)))
(define battleship (ship "battleship" 4 (list '() '() '() '() ) (list '() '() '() '() ) 0 0 (* 40 4)))
(define cruiser (ship "cruiser" 3 (list '() '() '() ) (list '() '() '() ) 0 0 (* 40 3)))
(define submarine (ship "submarine" 3 (list '() '() '() ) (list '() '() '() ) 0 0 (* 40 3)))
(define destroyer (ship "destroyer" 2 (list '() '() ) (list '() '() ) 0 0 (* 40 2)))

(define ship-list (list carrier battleship cruiser submarine destroyer))
(define ship-list-strings (for/list ([i ship-list]) (ship-name i)))

  
(define active-ships-p1 null)
(set! active-ships-p1 (list))

(define active-ships-p2 null)
(set! active-ships-p2 (list))

(define destroyed-ships-p1 null)
(set! destroyed-ships-p1 (list))

(define destroyed-ships-p2 null)
(set! destroyed-ships-p2 (list))

; Grid Functions
(define grid-flatten (lambda (grid) (flatten (for*/list ([i (range 0 10)]) (vector->list (list-ref (vector->list grid) i))))))

(define grid-list (lambda (grid) (hash->list grid #t)))

(define grid-keys (lambda (grid) (hash-keys grid #t)))

(define get-row-whole (lambda (y grid) (hash-ref grid y)))

(define get-row-index (for*/vector ([i (range 0 10)]) (vector i 0)))

(define get-active-grid (lambda (x)
                          (cond
                            ([equal? x "Player 1"] (displayln "Player 1") P1-GRID)
                            ([equal? x "Player 2"] (displayln "Player 2") P2-GRID)
                            )))

(define get-active-ht (lambda (x)
                        (cond
                          ([equal? x "Player 1"] grid-ht-p1)
                          ([equal? x "Player 2"] grid-ht-p2)
                          )))

(define set-state-grid (lambda (co-ord state grid-hash)
                         (let ([y (first co-ord)]
                               [x (second co-ord)])
                           (vector-set! (hash-ref grid-hash y) (- x 1) state))))

(define get-state-grid (lambda (co-ord grid)
                         (let ([y (first co-ord)]
                               [x (second co-ord)])
                         (vector-ref (hash-ref grid y) (- x 1)))))

(define return-input-int (lambda (y x index)
                           (cond
                             ([equal? index "x"]
                              (cond
                                ([equal? x 10] (+ x 1))
                                (else x)))
                             ([equal? index "y"]
                              (cond
                                ([equal? y "J"] (+ 2 (index-of (grid-keys grid-ht-p1) y)))
                                (else (+ 1 (index-of (grid-keys grid-ht-p1) y))))
                              )
                             )))

(define return-input-int-draw (lambda (y x)
                                (list (* 40 (return-input-int y x "y")) (* 40 (return-input-int y x "x")))))

; Ship Functions
(define set-ship-position (lambda (ship-name y x direction lst)
                            (cond
                              ([equal? direction "west"]
                                (cond
                                  ([equal? (length lst) (ship-length ship-name)] (cond
                                                                                   ([equal? active-player "Player 1"] (set-ship-P1-position! ship-name (reverse lst)))
                                                                                   ([equal? active-player "Player 2"] (set-ship-P2-position! ship-name (reverse lst)))))
                                  ([< x 1] "Can not place ship here")
                                  (else (set-ship-position ship-name y (+ x 1) "west" (cons (list y x) lst)))
                                  )
                                )
                              ([equal? direction "east"]
                                (cond
                                  ([equal? (length lst) (ship-length ship-name)] (cond
                                                                                   ([equal? active-player "Player 1"] (set-ship-P1-position! ship-name (reverse lst)))
                                                                                   ([equal? active-player "Player 2"] (set-ship-P2-position! ship-name (reverse lst)))))
                                  ([> x 10] "Can not place ship here")
                                  (else (set-ship-position ship-name y (- x 1) "east" (cons (list y x) lst)))
                                  )
                               )
                              ([equal? direction "south"]
                                (cond
                                  ([equal? (length lst) (ship-length ship-name)] (cond
                                                                                   ([equal? active-player "Player 1"] (set-ship-P1-position! ship-name (reverse lst)))
                                                                                   ([equal? active-player "Player 2"] (set-ship-P2-position! ship-name (reverse lst)))))
                                  (else (set-ship-position ship-name (list-ref (grid-keys (get-active-ht active-player)) (- (index-of (grid-keys (get-active-ht active-player)) y) 1)) x "south" (cons (list y x) lst)))
                                  )
                               )
                              ([equal? direction "north"]
                                (cond
                                  ([equal? (length lst) (ship-length ship-name)] (cond
                                                                                   ([equal? active-player "Player 1"] (set-ship-P1-position! ship-name (reverse lst)))
                                                                                   ([equal? active-player "Player 2"] (set-ship-P2-position! ship-name (reverse lst)))))
                                  (else (set-ship-position ship-name (list-ref (grid-keys (get-active-ht active-player)) (+ (index-of (grid-keys (get-active-ht active-player)) y) 1)) x "north" (cons (list y x) lst)))
                                  )
                                )
                              )))

(define add-ship-list (lambda (ship-name active-list)
                           (set! active-list (cons ship-name active-list))))

(define remove-ship-list (lambda (ship-name active-list)
                           (set! active-list (remove ship-name active-list))))

(define all-active-pos (lambda ()
                          (for/list ([i active-ships-p1])
                           (ship-P1-position i))))

; Draw Functions
(define set-ship-position-grid (lambda (ship-name position-arg)
                             (for ([i position-arg])
                               (set-state-grid i occupied-cell (get-active-ht active-player)))))

(define draw-ship-to-grid (lambda (ship-name y x direction)
                            (set-ship-position ship-name y x direction '())
                            (change-game-state)
                            (cond
                              ([equal? active-player "Player 1"] (set! active-ships-p1 (cons ship-name active-ships-p1)) (set-ship-position-grid ship-name (ship-P1-position ship-name)) (set-ship-P1-state! ship-name 1) (get-active-grid active-player) (change-active-player) P1-GRID)
                              ([equal? active-player "Player 2"] (set! active-ships-p2 (cons ship-name active-ships-p2)) (set-ship-position-grid ship-name (ship-P2-position ship-name)) (set-ship-P2-state! ship-name 1) (get-active-grid active-player) (change-active-player) P2-GRID))
                            )
  )

; Game State Functions
(define change-game-state (lambda ()
                            (cond
                              ([and (equal? (length active-ships-p1) 5) (equal? (length active-ships-p2) 5)](set! game-state (+ game-state 1)))
                              ([or (equal? (length destroyed-ships-p1) 5) (equal? (length destroyed-ships-p2) 5)] (set! game-state (+ game-state 1)) (won-game-state active-player))
                              )))

(define won-game-state (lambda (player) (cond
                                    ([equal? game-state 2] (displayln "Game Won") player))))
(define change-active-player (lambda ()
                               (cond
                                 ([equal? active-player "Player 1"] (set! active-player "Player 2"))
                                 ([equal? active-player "Player 2"] (set! active-player "Player 1"))
                                 )))

; Shoot Functions
(define shoot (lambda (y x player)
                (cond
                  ([equal? player "Player 1"]
                   (let ([active-hash grid-ht-p1]
                         [active-lst active-ships-p1])
                     (set-state-grid (list y x) destroyed-cell active-hash)
                     (ship-hit-check1 y x active-lst)
                     ))
                   
                  ([equal? player "Player 2"]
                   (let ([active-hash grid-ht-p2]
                         [active-lst active-ships-p2])
                     (set-state-grid (list y x) destroyed-cell active-hash)
                     (ship-hit-check1 y x active-lst)
                     ))
                  )
                )
  )

;; fix this - (ship-P1-positions (first active-ships-p1))
;; create list of all active positions to iterate through, use for loop or create new variable
(define ship-hit-check1 (lambda (y x active-lst)
                         (let ([pos-lst (ship-P1-position (first active-lst))])
                           (cond
                             ([empty? active-lst] "Miss")
                             ([empty? pos-lst] (ship-hit-check1 y x (rest active-lst)))
                             ([and (equal? active-player "Player 1") (equal? y (caar pos-lst)) (equal? x (cadar pos-lst))] "Ship Hit")
                             (else (ship-hit-check1 y x (rest active-lst)))
                             )
                           )
                         )
  )                        


; Computer/Player 2 Functions
