#lang racket
(require "../interp.rkt" "../asm/interp.rkt" rackunit)

(define (run e)
  (interp e))

;; Abscond examples
(check-equal? (run 7) 7)
(check-equal? (run -8) -8)

;; Blackmail examples
(check-equal? (run '(add1 (add1 7))) 9)
(check-equal? (run '(add1 (sub1 7))) 7)

;; Con examples
(check-equal? (run '(if (zero? 0) 1 2)) 1)
(check-equal? (run '(if (zero? 1) 1 2)) 2)
(check-equal? (run '(if (zero? -7) 1 2)) 2)
(check-equal? (run '(if (zero? 0)
                        (if (zero? 1) 1 2)
                        7))
              2)
(check-equal? (run '(if (zero? (if (zero? 0) 1 0))
                        (if (zero? 1) 1 2)
                        7))
              7)

;; Con+ examples
(check-equal? (run '(abs 10)) 10)
(check-equal? (run '(abs -10)) 10)
(check-equal? (run '(- 10)) -10)
(check-equal? (run '(- -10)) 10)
(check-equal? (run '(- (- 10))) 10)
(check-equal? (run '(cond [else 5])) 5)
(check-equal? (run '(cond [(zero? 1) 2] [else 3])) 3)
(check-equal? (run '(cond [(zero? 0) 2] [else 3])) 2)
(check-equal? (run '(cond [(zero? 1) 2] [(zero? (sub1 1)) 4] [else 3])) 4)

