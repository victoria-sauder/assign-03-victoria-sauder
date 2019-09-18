#lang racket
(require "../interp.rkt" "../compile.rkt" "../asm/interp.rkt" "../random.rkt" rackunit)

(define (check-compiler e)
  (check-eqv? (asm-interp (compile e))
              (interp e)
              e))

(for ([i (in-range 500)])
  (check-compiler (random-expr)))

