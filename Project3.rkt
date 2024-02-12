#lang racket/gui

(define WIDTH 400)
(define HEIGHT 400)
(define SPACE 50)
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

; GUI
(define mainFrame (new frame%
                       [label "Snake"]
                       [min-width WIDTH]
                       [min-height HEIGHT]
                       [stretchable-width #f]
                       [stretchable-height #f]))



(send mainFrame show #t)