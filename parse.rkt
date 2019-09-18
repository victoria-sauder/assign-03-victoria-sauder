#lang racket
(provide parse)

;; type Token =
;; | Integer
;; | 'add1
;; | 'sub1
;; | 'zero?
;; | 'if
;; | 'cond
;; | 'else
;; | 'abs
;; | '-
;; | 'lparen    ;; (
;; | 'rparen    ;; )
;; | 'lsquare   ;; [
;; | 'rsquare   ;; ]
;; | 'eof       ;; end of file

;; (Listof Token) -> Expr
(define (parse lot)
  ;; TODO
  0
)



(module+ test
  (require rackunit)
  (require "lex.rkt")
  ;; String -> Expr
  (define (p s)
    (parse (lex-string (string-append "#lang racket " s))))
  
  (check-equal? (p "7") 7)
  (check-equal? (p "(add1 7)") '(add1 7))
  (check-equal? (p "(sub1 7)") '(sub1 7))
  (check-equal? (p "[add1 7]") '(add1 7))
  (check-equal? (p "[sub1 7]") '(sub1 7))
  (check-equal? (p "(abs 7)") '(abs 7))
  (check-equal? (p "[abs 7]") '(abs 7))  (check-equal? (p "(- 7)") '(- 7))
  (check-equal? (p "[- 7]") '(- 7))
  (check-equal? (p "(cond [else 1])") '(cond [else 1]))
  (check-equal? (p "(cond [(zero? 0) 2] [else 1])")
                '(cond [(zero? 0) 2] [else 1]))
  (check-equal? (p "(cond [(zero? 0) 2] [(zero? 1) 3] [else 1])")
                '(cond [(zero? 0) 2] [(zero? 1) 3] [else 1]))
  (check-equal? (p "(cond [(zero? 0) 2] [(zero? 1) 3] (else 1))")
                '(cond [(zero? 0) 2] [(zero? 1) 3] [else 1]))
  (check-equal? (p "(if (zero? 9) 1 2)")
                '(if (zero? 9) 1 2))
  ;; TODO: add more tests
  #;...)
