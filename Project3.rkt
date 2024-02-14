#lang racket/gui

(define WIDTH 400)
(define HEIGHT 400)
(define SPACE 25)
(define START_PARTS 3)
(define ROW (/ HEIGHT SPACE))
(define COLUMN (/ WIDTH SPACE))

(define snake%
  (class object%
    (init-field (body_parts START_PARTS)
                (body_coords (make-vector 0))
                (body_squares (make-vector 0))
                )
    )
  )

(define apple%
  (class object%
    (init-field (apple_spawn (make-vector 2)) ; (random (exact-round (/ WIDTH SPACE))) & (random (exact-round (/ HEIGHT SPACE)))
                (

; GUI
(define mainFrame (new frame%
                       [label "Snake"]
                       [min-width WIDTH]
                       [min-height HEIGHT]
                       [stretchable-width #f]
                       [stretchable-height #f]))



(send mainFrame show #t)