#lang racket

(provide GRID_SIZE
         FRAME_SIZE
         SCALE
         START_PARTS
         ROW
         COLUMN)

(define GRID_SIZE 5)
(define FRAME_SIZE (vector 600 400))
(define SCALE 25)
(define START_PARTS 3)
(define ROW (/ (vector-ref FRAME_SIZE 0) SCALE))
(define COLUMN (/ (vector-ref FRAME_SIZE 1) SCALE))