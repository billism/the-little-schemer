#lang scheme

(require "ch04.scm")
(provide (all-defined-out))

; from Chapter 1
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

; whether a representation of an arithmetic expression
; contains only numbers besides the +,x,^
(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else
       (and (numbered? (car aexp))
            (numbered? (car (cdr (cdr aexp)))))))))

; the following are for representations of the form (+ 1 2)
; we need some helper functions
(define 1st-sub-exp
  (lambda (aexp)
     (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))


(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator nexp) (quote +))
       (plus (value (1st-sub-exp nexp))
             (value (2nd-sub-exp nexp))))
      ((eq? (operator nexp) (quote *))
       (times (value (1st-sub-exp nexp))
              (value (2nd-sub-exp nexp))))
      (else
       (pow (value (1st-sub-exp nexp))
            (value (2nd-sub-exp nexp)))))))

; Then we consider an abstract natural number system, and define zero?, add1, sub1 etc.
(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons (quote ()) n)))

(define zub1
  (lambda (n)
    (cdr n)))

(define splus
  (lambda (n m)
    (cond
      ((sero? m) n)
      (else
       (edd1 (splus n (zub1 m)))))))

