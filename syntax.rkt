#lang racket
(provide (all-defined-out))

;; Any -> Boolean
;; Is x a well-formed expression?
(define (expr? x)
  (match x
    [(? integer? i) #t]
    [`(add1 ,x) (expr? x)]
    [`(sub1 ,x) (expr? x)]
    [`(if (zero? ,x) ,y ,z)
     (and (expr? x)
          (expr? y)
          (expr? z))]
    [`(abs ,x) (expr? x)]
    [(cons 'cond t) (cond-expr? (cdr t))]
    [`(- ,x) (expr? x)]
    [_ #f]))

(define (cond-expr? x)
  (if (= (length x) 1)
      (match (first x)
        [`(else ,y) (expr? y)]
        [_ #f])
      (match (first x)
        [`((zero? ,y) ,z) (and (expr? y) (expr? z)
                               ;(cond-expr? (cdr x))
                               )]
        [_ #f])
      )
)
    

