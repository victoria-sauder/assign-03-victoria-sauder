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

;; <expr> ::= integer
;; | ( <compound> )
;; | [ <compound> ]

;; <compound> ::= <prim> <expr>
;; | if <question> <expr> <expr>
;; | cond <clause>* <else>

;; <prim> ::= add1 | sub1 | abs | -

;; <clause> ::= ( <question> <expr> )
;; | [ <question> <expr> ]

;; <question> ::= ( zero? <expr> )
;; | [ zero? <expr> ]

;; <else> ::= ( else <expr> )
;; | [ else <expr> ]

(define *input* (box '()))

;; (Listof Token) -> Expr
(define (parse lot)
  (set-box! *input* lot)
  (let ((e (parse-expr!))
        (_ (match-tok! 'eof)))
    e))

; -> Expr
; EFFECT: consume one expression's worth of tokens
(define (parse-expr!)
  (match (look-ahead)
    [(? integer? i)
     (match-tok! i)]
    ['lparen (let ([.. (match-tok! 'lparen)]
                   [e (parse-compound!)]
                   [... (match-tok! 'rparen)]) e)]
    ['lsquare (let ([.. (match-tok! 'lsquare)]
                    [e (parse-compound!)]
                    [... (match-tok! 'rsquare)]) e)]))

; -> Expr
; EFFECT: consume one expression's worth of tokens
(define (parse-compound!)
  (match (look-ahead)
    ['add1 (let ([e1 (match-tok! 'add1)]
                 [e2 (parse-expr!)]
                 ) (cons 'add1 (cons e2 '() )))]
    ['sub1 (let ([e1 (match-tok! 'sub1)]
                 [e2 (parse-expr!)]
                 ) (cons 'sub1 (cons e2 '() )))]
    ['abs (let ([e1 (match-tok! 'abs)]
                [e2 (parse-expr!)]
                ) (cons 'abs (cons e2 '() )))]
    ['- (let ([e1 (match-tok! '-)]
              [e2 (parse-expr!)]
              ) (cons '- (cons e2 '() )))]
    ['if (let ([e1 (match-tok! 'if)]
               [e2 (parse-question!)]
               [e3 (parse-expr!)]
               [e4 (parse-expr!)])
           (cons e1 (cons e2 (cons e3 (cons e4 '())))))]
    ['cond (let ([e1 (match-tok! 'cond)]
                 [e2 (parse-clause!)]) (append (cons 'cond '()) e2))]
    ))



; -> Expr
; EFFECT: consume one expression's worth of tokens
(define (parse-clause!)
  (match (look-ahead2)
    ['else (parse-else!)]
    [_  (match (look-ahead)
          ['lparen (let ([e0 (match-tok! 'lparen)]
                         [e1 (parse-question!)]
                         [e2 (parse-expr!)]
                         [e3 (match-tok! 'rparen)]
                         [e4 (match (look-ahead2)
                               ['else (parse-else!)]
                               [_ (parse-clause!)])])
                     (cons (cons e1 (cons e2 '())) e4)
                     )]
          ['lsquare (let ([e0 (match-tok! 'lsquare)]
                          [e1 (parse-question!)]
                          [e2 (parse-expr!)]
                          [e3 (match-tok! 'rsquare)]
                          [e4 (match (look-ahead2)
                                ['else (parse-else!)]
                                [_ (parse-clause!)])])
                      (cons (cons e1 (cons e2 '())) e4)
                      )]
          )]
))

; -> Expr
; EFFECT: consume one expression's worth of tokens
(define (parse-question!)
  (match (look-ahead)
    ['lparen (let ([e1 (match-tok! 'lparen)]
                   [e2 (match-tok! 'zero?)]
                   [e3 (parse-expr!)]
                   [e4 (match-tok! 'rparen)]) (cons 'zero? (cons e3 '())))]
    ['lsquare (let ([e1 (match-tok! 'lsquare)]
                    [e2 (match-tok! 'zero?)]
                    [e3 (parse-expr!)]
                    [e4 (match-tok! 'rsquare)]) (cons 'zero? (cons e3 '())))]
    ))

; -> Expr
; EFFECT: consume one expression's worth of tokens
(define (parse-else!)
  (match (look-ahead)
    ['lparen (let ([e1 (match-tok! 'lparen)]
                   [e2 (match-tok! 'else)]
                   [e3 (parse-expr!)]
                   [e4 (match-tok! 'rparen)]) (cons (cons 'else (cons e3 '())) '()))]
    ['lsquare (let ([e1 (match-tok! 'lsquare)]
                    [e2 (match-tok! 'else)]
                    [e3 (parse-expr!)]
                    [e4 (match-tok! 'rsquare)]) (cons (cons 'else (cons e3 '())) '()))]
    ))


; -> Token
; Produce (but don't consume) the next token
(define (look-ahead)
  (match (unbox *input*)
    ['() (error "no look ahead available")]
    [(cons t _) t]))

; -> Token
; Produce (but don't consume) the next token
(define (look-ahead2)
  (match (unbox *input*)
    ['() (error "no look ahead available")]
    [(cons t '()) (error "no look ahead2 available")]
    [(cons t (cons t0 _)) t0]))
 
; Token -> Token
; EFFECT: consumes one token of input
(define (match-tok! t)
  (match (unbox *input*)
    ['() (error "no token available")]
    [(cons next ts)
     (set-box! *input* ts)
     (unless (equal? t next)
       (error "parse error"))
     t]))


(require "lex.rkt")
;; String -> Expr
(define (p s)
  (parse (lex-string (string-append "#lang racket " s))))

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
