#lang racket/gui
(require dyoo-while-loop)
(require "functions.rkt")
(range 20 420 40)
(range 0 400 40)
(define square-size 40)
(define coords(for*/vector ([i (range 20 420 40)] [j (range 20 420 40)]) (vector i j)))

(define draw-grid (λ (x-start y-start x-extent y-extent) (sleep/yield 0.001)(for* ([i (range x-start x-extent 40)] [j (range y-start y-extent 40)]) (send dc draw-rectangle i j 40 40))
                    (for ([i  (range y-start y-extent 40) ] [j (build-string 10 (lambda (i) (integer->char (+ i 97))))]) (send dc draw-text (string j) (- x-start 20) i))
                    (for ([i  (range x-start x-extent 40) ] [j (build-list 10 (lambda (x) (+ x 1)))]) (send dc draw-text (number->string j) i y-extent))))
(define draw-ship(λ (x-start y-start x-extent y-extent direction) (sleep/yield 0.001)(cond ((equal? direction "east")(send dc set-brush "black" 'transparent)(send dc set-pen "red" 5 'solid)(send dc draw-rectangle x-start y-start x-extent y-extent))
                                                                        ((equal? direction "south")(send dc set-brush "black" 'transparent)(send dc set-pen "red" 5 'solid)(send dc draw-rectangle x-start y-start y-extent x-extent))
                                                                        ((equal? direction "west")(send dc set-brush "black" 'transparent)(send dc set-pen "red" 5 'solid)(send dc draw-rectangle (- x-start (- x-extent 40)) y-start x-extent y-extent))
                                                                        )))
(define frame (new frame% [label "Example"]
                   [alignment '(left top)]
                   
                   [min-width 1550]	 
   	 	[min-height 730]
                [stretchable-width #f]
                [stretchable-height #f]))
(define panel (new panel%  [parent frame]
                   [alignment '(left top)]
                   
                 ))
(define panel2 (new horizontal-panel%  [parent frame]
              
                   [min-height 50]
     [min-width 100]
     [stretchable-width #t]	 
     [stretchable-height #f]
                 ))
(define bmp (make-object bitmap% 444 277))
(send bmp load-file "/Users/thoma/Downloads/sad.bmp")
(define my-canvas%
  (class canvas% 
    (define/override (on-event mouse-event) (cond ((equal? (send mouse-event get-event-type) 'right-down)
                                                   (send dc draw-ellipse (send mouse-event get-x) (send mouse-event get-y) 20 20))
                                                  ((equal? (send mouse-event get-event-type) 'left-down) (send panel set-cursor (make-object cursor% 'hand))(for ([i coords])
                                                  (cond    ((and (< (send mouse-event get-x) (+ (vector-ref i 0) 40)) (> (send mouse-event get-x) (vector-ref i 0))  
                                                        (> (send mouse-event get-y) (vector-ref i 1))   (< (send mouse-event get-y) (+ (vector-ref i 1) 40)))
                                                            (send dc set-pen "red" 3 'solid)(send dc set-brush "black" 'transparent)(send dc draw-rectangle (vector-ref i 0) (vector-ref i 1) 40 40))
                                                           ) ))
                                                  (else (send panel set-cursor (make-object cursor% 'arrow)))
                                                  ))
    (super-new)))

(define x(new my-canvas% [parent panel]
    
     [paint-callback
      (lambda (canvas dc)
        (send dc set-brush (new brush% [color "white"]))
        (send dc set-pen "black" 0.5 'solid)
        (send dc set-background "red")
        (send dc set-font (make-font #:size 11 #:family 'modern #:weight 'normal))
        (draw-grid 20 20 420 420)
        (draw-grid 520 20 900 420)
        (send dc draw-text "Player 🧍" 20 0)
        (send dc set-font (make-font #:size 20 #:family 'swiss #:weight 'normal #:underlined? #f))
        (send dc set-text-mode 'solid)
        (send dc set-text-background "medium sea green")
        (send dc set-text-foreground "white")
        (send dc draw-text "🌊 wee" 180 40)
        (send dc draw-text "🤖" 100 100)
        (send dc set-text-mode 'transparent)
        (send dc set-text-foreground "orange red")
        (send dc set-font (make-font #:size 15 #:family 'swiss #:weight 'normal 	#:underlined? #f))
        (send dc draw-text "💥" 380 360)
        (for/list ([i (append (list "Player 1") ship-list)] [j (range 480 680 20)]) (send dc draw-text i 20 j))
        (send dc set-font (make-font #:size 15 #:family 'modern #:weight 'normal))
        (send dc set-text-foreground "black")
        (send dc set-pen "black" 0.5 'solid)
        )])
  )

(define ship_choices ship-list)
(define choice(new choice% [parent panel2]
       [label  ""]
       [choices ship_choices]
       [min-width 50]	 
   	 	[min-height 30]
       	[stretchable-width #f]	 
   	 	[stretchable-height #t]
       
       ))
(define choice2(new choice% [parent panel2]
       [label  ""]
       [choices '("north" "east" "south" "west")]
       [min-width 50]	 
   	 	[min-height 30]
       	[stretchable-width #f]	 
   	 	[stretchable-height #t]
       
       ))
(define text-field (new text-field%
                        (label "")
                        (parent panel2)
                        [stretchable-width #f]
                        (init-value "")))
(define button (new button%
                    (parent panel2)
                    (label "place")
                    (callback (lambda (button event) (cond ((equal? (send choice get-string-selection) "carrier")(send choice delete (send choice get-selection))(draw-ship 20 20 (ship-cumulative carrier) square-size (send choice2 get-string-selection)))
                                                           ((equal? (send choice get-string-selection) "battleship")
                                                            (send choice delete (send choice get-selection))
                                                            (send choice set-selection (random (send choice get-number)))
                                                            (draw-ship
                                                             (vector-ref (vector-ref (get-row-whole (string (string-ref (list->string (filter (λ (x) (not (equal? x #\space))) (string->list (send text-field get-value)))) 0)) grid-ht-p1)
                                                                         (- (string->number (string (string-ref (list->string (filter (λ (x) (not (equal? x #\space))) (string->list (send text-field get-value)))) 1))) 1)) 0)
                                                             (vector-ref (vector-ref (get-row-whole (string (string-ref (list->string (filter (λ (x) (not (equal? x #\space))) (string->list (send text-field get-value)))) 0)) grid-ht-p1)
                                                                         (- (string->number (string (string-ref (list->string (filter (λ (x) (not (equal? x #\space))) (string->list (send text-field get-value)))) 1))) 1)) 1)
                                                             
                                                             (ship-cumulative battleship) square-size (send choice2 get-string-selection)))
                                                           )))))
(filter (λ (x) (not (equal? x #\space))) (string->list (send text-field get-value)))
;(string-ref (list->string (filter (λ (x) (not (equal? x #\space))) (string->list (send text-field get-value)))) 0)
(define text (new text%))
(send text insert "Editor Canvas")
;(send editor-canvas set-editor text)
(define dc (send x get-dc))
(send frame show #t)
(send text get-text)
