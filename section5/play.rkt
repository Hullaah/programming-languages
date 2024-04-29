#lang racket

(provide (all-defined-out))

(filter (
    lambda (x)  x 0) 
))

(foldl (lambda (x y) (+ x y)) 0 '[1 2 3 4 5])
