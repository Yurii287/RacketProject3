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
    (init-field (apple_spawn (make-vector 2)) 
                (apple_squares (make-vector 0))
                (apple_color "Red")
                )
    )
  )

; Functions
(define rand-square (lambda ()
                      (cons (random 0 (exact-round (/ (car FRAME_SIZE) SCALE))) (random 0 (exact-round (/ (cdr FRAME_SIZE) SCALE))))))

(define draw-square (lambda (pos dc)
                      (let ([x (first pos)]
                               [y (second pos)])
                        (send dc draw-rectangle x y 5 5))))

; GUI
(define mainFrame (new frame%
                       [label "Snake"]
                       [min-width (car FRAME_SIZE)]
                       [min-height (cdr FRAME_SIZE)]
                       [stretchable-width #f]
                       [stretchable-height #f]))



(send mainFrame show #f)