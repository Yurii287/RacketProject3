#lang racket

(provide GRID_SIZE
         FRAME_SIZE
         SCALE
         START_PARTS
         ROW
         COLUMN)

(define GRID_SIZE 5)
(define FRAME_SIZE (cons 600 400))
(define SCALE 25)
(define START_PARTS 3)
(define ROW (/ (cdr FRAME_SIZE) SCALE))
(define COLUMN (/ (car FRAME_SIZE) SCALE))
