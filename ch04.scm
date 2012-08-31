#lang scheme

(require "ch03.scm")
(provide (all-defined-out))

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define plus
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (plus n (sub1 m)))))))

(define minus
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (minus n (sub1 m)))))))

; a tuple is just a list of numbers
(define addup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (plus (car tup) (addup (cdr tup)))))))

(define times
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (plus n (times n (sub1 m)))))))

; element-wise addition
(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (plus (car tup1) (car tup2))
                  (tup+ (cdr tup1) (cdr tup2)))))))

; recall that we only consider natural numbers
(define gt
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (gt (sub1 n) (sub1 m))))))

(define lt
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (lt (sub1 n) (sub1 m))))))

;(define eq
;  (lambda (n m)
;    (cond
;      ((zero? n) (zero? m))
;      ((zero? m) #f)
;      (else (eq (sub1 n) (sub1 m))))))

; "eqnum?" for atoms that are numbers
(define eqnum?
  (lambda (n m)
    (cond
      ((gt n m) #f)
      ((lt n m) #f)
      (else #t))))

(define pow
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (times n (pow n (sub1 m)))))))

(define div
  (lambda (n m)
    (cond
      ((lt n m) 0)
      (else (add1 (div (minus n m) m))))))

(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))

; one-based indexing
(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat)
                  (rempick (sub1 n) (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((number? (car lat)) (no-nums (cdr lat)))
              (else (cons (car lat) (no-nums (cdr lat)))))))))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((number? (car lat))
               (cons (car lat) (all-nums (cdr lat))))
              (else (all-nums (cdr lat))))))))

; check if two atoms are the same
(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2))
       (eqnum? a1 a2))
      ((or (number? a1) (number? a2))
       #f)
      (else (eq? a1 a2)))))

; count the number of occurence
(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      (else (cond
              ((eq? (car lat) a) (add1 (occur a (cdr lat))))
              (else (occur a (cdr lat))))))))

;(define one?
;  (lambda (n)
;    (cond
;      ((zero? n) #f)
;      (else (one? (sub1 n))))))

(define one?
  (lambda (n)
    (eqnum? n 1)))

;(define rempick
;  (lambda (n lat)
;    (cond
;      ((one? n) (car lat))
;      (else (cons (car lat)
;                  (rempick (sub1 n)
;                           (cdr lat)))))))