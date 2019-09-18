#lang racket
(provide (all-defined-out))

;; This assignment should be completed individually.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I pledge on my honor that I have not given or received any
;; unauthorized assistance on this assignment.
;;
;; Name: zz
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Expr -> Asm
(define (compile e)
  `(entry
    ,@(compile-e e)
    ret))

;; Expr -> Asm
(define (compile-e e)
  (match e
    [(? integer? i) `((mov rax ,i))]
    [`(add1 ,e0)
     (let ((c0 (compile-e e0)))
       `(,@c0
         (add rax 1)))]    
    [`(sub1 ,e0)
     (let ((c0 (compile-e e0)))
       `(,@c0
         (sub rax 1)))]
    [`(if (zero? ,e0) ,e1 ,e2)
     (let ((c0 (compile-e e0))
           (c1 (compile-e e1))
           (c2 (compile-e e2))
           (l0 (gensym "if"))
           (l1 (gensym "if")))
       `(,@c0
         (cmp rax 0)
         (jne ,l0)
         ,@c1
         (jmp ,l1)
         ,l0
         ,@c2
         ,l1))]
    [`(abs ,e0)
     (let ([c0 (compile-e e0)])
       `(,@c0
         (mov rbx\, rax)
         (neg rax)
         (cmovl rax\, rbx)))]
    [`(- ,e0)
     (let ([c0 (compile-e e0)])
       `(,@c0
         (neg rax)))]
    [`(cond (else ,e0))
     (let ([c0 (compile-e e0)])
       `(,@c0))]
    [(cons 'cond t) (compile-cond t)]
    ))

(define (compile-cond t)
  (match (first t)
    [`((zero? ,x) ,y) (let ([c0 (compile-e x)]
                            [c1 (compile-e y)]
                            [l0 (gensym "cond")]
                            [l1 (gensym "cond")]
                            [t0 (compile-cond (cdr t))])
                        `(,@c0
                          (cmp rax 0)
                          (jne ,l0)
                          ,@c1
                          (jmp ,l1)
                          ,l0
                          ,t0
                          ,l1))]
    [`(else ,x) (let ([c0 (compile-e x)])
                  `(,@c0))]))

'((mov rax 0) (cmp rax 0) (jne cond69068) (mov rax 2) (jmp cond69069) cond69068 ((mov rax 1)) cond69069)