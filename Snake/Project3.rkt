#lang racket

(require racket/gui
         racket/draw
         "settings.rkt")

; Classes
(define snake%
  (class object%
    (super-new)
    (init-field (body_parts START_PARTS)
                (body_coords (make-vector 0))
                (body_squares (make-vector 0))
                (body_color "green")
                )
    (define/public get-body-color (λ () body_color))
    (define/public get-body-coords (λ () body_coords))
    (define/public get-body-squares (λ () body_squares))
    )
  )

(define apple%
  (class object%
    (super-new)
    (init-field (apple_spawn (make-vector 2)) 
                (apple_squares (make-vector 0))
                (apple_color "red")
                )
    (define/public get-apple-color (λ () apple_color))
    (define/public get-apple-spawn (λ () apple_spawn))
    (define/public get-apple-squares (λ () apple_squares))
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

(define gameScreen (new canvas%
                        [parent mainFrame]))
  

; Variables
(define gameCanvas (send gameScreen get-dc))
(define snake (new snake%))
(define apple (new apple%))

(send mainFrame show #t)