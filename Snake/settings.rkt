#lang racket

(provide GRID_SIZE
         FRAME_SIZE
         SCALE
         START_PARTS
         ROW
         COLUMN)

(define GRID_SIZE 5)
(define FRAME_SIZE (cons 400 400))
(define SCALE 25)
(define START_PARTS 3)
(define ROW (/ HEIGHT SPACE))
(define COLUMN (/ WIDTH SPACE))
