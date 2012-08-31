#lang scheme

(require "ch02.scm")
(provide (all-defined-out))


; The function "rember" takes an atom and a lat as
; its arguments and makes a new lat with the first
; occurrence of the atom in the old lat removed
(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat)
                  (rember a (cdr lat)))))))

;(rember 's '())
;(rember 's '(a s d))

; The function "firsts" takes one argument, a list,
; which is either a null list or contains only non-empty
; lists. It builds another list composed of the first
; S-expression of each internal list.
(define firsts
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      (else (cons (car (car l))
                  (firsts (cdr l)))))))

;(firsts '())
;(firsts '((a s d) (c s d) (v s d)))

; The function "insertR" takes three arguments: the atom "new"
; and "old", and a lat. It builds a lat with "new" inserted
; to the right of the first occurrence of "old".
(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old)
       (cons old
             (cons new (cdr lat))))
      (else (cons (car lat)
                  (insertR new old (cdr lat)))))))

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old)
       (cons new lat))
      (else (cons (car lat)
                  (insertL new old (cdr lat)))))))

; The function "subst" substitutes the first occurrence
; of "old" in the "lat" with "new".
(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((eq? (car lat) old)
               (cons new (cdr lat)))
              (else (cons (car lat)
                          (subst new old (cdr lat)))))))))
; The function "subst2" substitutes either the first
; occurence of "o1" or the first occurence of "o2" by "new".
(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((or (eq? (car lat) o1) (eq? (car lat) o2))
               (cons new (cdr lat)))
              (else (cons (car lat)
                          (subst2 new o1 o2 (cdr lat)))))))))

; The function "multirember" takes an atom "a" and a lat as
; its arguments. It makes new list with all occurrences of
; "a" removed.
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((eq? (car lat) a) (multirember a (cdr lat)))
              (else (cons (car lat)
                          (multirember a (cdr lat)))))))))

; 
(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((eq? old (car lat))
               (cons old
                     (cons new
                           (multiinsertR new old (cdr lat)))))
              (else (cons (car lat)
                          (multiinsertR new old (cdr lat)))))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((eq? old (car lat))
               (cons new
                     (cons old (multiinsertL new old (cdr lat)))))
              (else (cons (car lat)
                          (multiinsertL new old (cdr lat)))))))))

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((eq? old (car lat))
               (cons new
                     (multisubst new old (cdr lat))))
              (else (cons (car lat)
                          (multisubst new old (cdr lat)))))))))