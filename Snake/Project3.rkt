#lang racket

(require racket/gui
         racket/draw
         "settings.rkt")

; Classes
(define snake%
  (class object%
    (init-field (body_parts START_PARTS)
                (body_coords (make-vector 0))
                (body_squares (make-vector 0))
                (body_color "Green")
                )
    )
  )

(define apple%
  (class object%
    (init-field (apple_spawn (make-vector 2)) ; (random (exact-round (/ WIDTH SPACE))) & (random (exact-round (/ HEIGHT SPACE)))
                (apple_squares (make-vector 0))
                (apple_color "Red")
                )
    )
  )

; Functions

(define rand-square (cons (random (exact-round (/ WIDTH SPACE))) (random (exact-round (/ HEIGHT SPACE)))))

(define draw-square (lambda (pos dc)
                      (let ([x (first pos)]
                               [y (second pos)])
                        (send dc draw-rectangle x y 5 5))))

; GUI
(define mainFrame (new frame%
                       [label "Snake"]
                       [min-width WIDTH]
                       [min-height HEIGHT]
                       [stretchable-width #f]
                       [stretchable-height #f]))



(send mainFrame show #t)