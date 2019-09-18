#lang racket
(provide (all-defined-out))

;; Expr -> Integer
(define (interp e)
  (match e
    [(? integer? i) i]
    [`(add1 ,e0)
     (+ (interp e0) 1)]
    [`(sub1 ,e0)
     (- (interp e0) 1)]
    [`(if (zero? ,e0) ,e1 ,e2)
     (if (zero? (interp e0))
         (interp e1)
         (interp e2))]
    [`(abs ,e0)
     (abs (interp e0))]
    [`(- ,e0)
     (- (interp e0))]
    [(cons 'cond t) (interp-cond t)]
    ))

(define (interp-cond e)
  (match (first e)
    [`((zero? ,x) ,y) (if (zero? (interp x)) (interp y) (interp-cond (cdr e)))]
    [`(else ,x) (interp x)]))
    
    
