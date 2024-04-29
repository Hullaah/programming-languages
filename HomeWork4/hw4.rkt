
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (cond
    [(< n 0) (error "list-nth-mod: negative number")]
    [(null? xs) (error "list-nth-mod: empty list")]
    [#t (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

(define (funny-number-stream)
  (letrec ([f (lambda (x)
                (cons (if (= (remainder x 5) 0)
                          (- 0 x)
                          x)
                      (lambda () (f (+ x 1)))))])
    (f 1)))

(define (dan-then-dog)
  (letrec ([f (lambda (x)
                (cons (if (= (remainder x 2) 0) "dan.jpg" "dog.jpg")
                      (lambda () (f (+ x 1)))))])
    (f 0)))

(define (stream-add-zero s)
  (lambda ()
    (cons (cons 0 (car (s))) (stream-add-zero (cdr (s))))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (x)
               (cons (cons
                      (list-nth-mod xs x)
                      (list-nth-mod ys x))
                     (lambda () (f (+ x 1)))))])
    (lambda () (f 0))))

(define (vector-assoc v vec)
  (letrec ([f (lambda (x)
             (cond
               [(= x (vector-length vec)) #f]
               [(pair? (vector-ref vec x))
                       (if (equal? (car (vector-ref vec x)) v)
                           (vector-ref vec x)
                           (f (+ 1 x)))]
               [#t (f (+ 1 x))]))])
    (f 0)))

(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [cache-end 0]
           [cached (lambda (x pos)
                     (cond
                       [(>= pos n) #f]
                       [(not (vector-ref cache pos)) #f]
                       [(equal? (car (vector-ref cache pos)) x) pos]
                       [#t (cached x (+ 1 pos))]))]
           [f (lambda (x)
                (let ([res (cached x 0)])
                  (if res (vector-ref cache res)
                      (let ([ans (assoc x xs)])
                        (begin
                          (set! cache-end (if (>= cache-end n) 0 cache-end))
                          (vector-set! cache cache-end ans)
                          (set! cache-end (+ 1 cache-end)) ans)))))])
  f))
