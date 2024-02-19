#lang racket

(require racket/gui
         racket/draw
         dyoo-while-loop
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

(define draw-square (lambda (dc pos)
                      (let ([x (vector-ref pos 0)]
                               [y (vector-ref pos 1)])
                        (send dc draw-rectangle x y 25 25))))

(define color-square (lambda (dc brush)
                       (send dc set-brush brush)))

; GUI
(define mainFrame (new frame%
                       [label "Snake"]
                       [min-width (vector-ref FRAME_SIZE 0)]
                       [min-height (vector-ref FRAME_SIZE 1)]
                       [stretchable-width #f]
                       [stretchable-height #f]))

(define gameScreen (new canvas%
                        [parent mainFrame]))
  

; Variables
; World variables
(define gameCanvas (send gameScreen get-dc))
; Snake variables
(define snake (new snake%))
(define snake_brush (new brush% [color "green"]))
(define snake_start (vector 0 0))
; Apple Variables
(define apple (new apple%))
(define apple_brush (new brush% [color "apple"]))

(send mainFrame show #t)