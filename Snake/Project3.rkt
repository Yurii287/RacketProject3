#lang racket

(require racket/gui
         racket/draw
         dyoo-while-loop
         "settings.rkt")

; Classes
(define game-canvas%
  (class canvas%
    (define/override (on-char key-event)
      (while (equal? (send key-event get-key-code) #\return)
              (color-square gameCanvas snake_brush)
              (vector-set! (send snake get-body-coords) 0 snake_start)
              (vector-set! (send snake get-body-coords) 1 (vector (- (vector-ref snake_start 0) SCALE) (vector-ref snake_start 1)))
              (vector-set! (send snake get-body-coords) 2 (vector (- (vector-ref snake_start 0) (* 2 SCALE)) (vector-ref snake_start 1)))
              (break))
      (while (equal? (send key-event get-key-code) 'up)
             (send snake set-snake-direction 'up)
             (for ([i (send snake get-body-coords)])
               (draw-square gameCanvas (vector (vector-ref i 0) (vector-ref i 1))))
             (break))
      )
    (super-new)))

(define snake%
  (class object%
    (super-new)
    (init-field (body_parts START_PARTS)
                (body_coords (for/vector ([i body_parts]) (make-vector 2)))
                (body_color "green")
                (direction 'up)
                )
    (define/public get-body-color (λ () body_color))
    (define/public get-body-coords (λ () body_coords))
    (define/public get-snake-direction (λ () direction))
    (define/public set-snake-direction (λ (x) (set! direction x)))
    (define/public set-body-coords (λ (x y) (vector-set! body_coords x y)))
    (define/public add-body-coords (λ (x) (vector-append body_coords x)))
    )
  )

(define apple%
  (class object%
    (super-new)
    (init-field (apple_spawn (make-vector 2)) 
                (apple_color "red")
                )
    (define/public get-apple-color (λ () apple_color))
    (define/public get-apple-spawn (λ () apple_spawn))
    )
  )

; Functions
(define rand-square (lambda ()
                      (vector (* (random 0 (exact-round (vector-ref FRAME_SIZE 0 ))) SCALE) (* (random 0 (exact-round (vector-ref FRAME_SIZE 1))) SCALE))))

(define draw-square (lambda (dc pos)
                      (let ([x (vector-ref pos 0)]
                               [y (vector-ref pos 1)])
                        (send dc draw-rectangle x y SCALE SCALE))))

(define color-square (lambda (dc brush)
                       (send dc set-brush brush)))

; GUI
(define mainFrame (new frame%
                       [label "Snake"]
                       [min-width (vector-ref FRAME_SIZE 0)]
                       [min-height (vector-ref FRAME_SIZE 1)]
                       [stretchable-width #f]
                       [stretchable-height #f]))

(define gameScreen (new game-canvas%
                        [parent mainFrame]))
  
; Variables
; World variables
(define gameCanvas (send gameScreen get-dc))
; Snake variables
(define snake (new snake%))
(define snake_brush (new brush% [color (send snake get-body-color)]))
(define snake_start (vector (exact-round (/ (vector-ref FRAME_SIZE 0) 2)) (exact-round (/ (vector-ref FRAME_SIZE 1) 2))))
; Apple Variables
(define apple (new apple%))
(define apple_brush (new brush% [color (send apple get-apple-color)]))
(send mainFrame show #t)
