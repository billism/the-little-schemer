#lang scheme

(require "ch04.scm")
(provide (all-defined-out))

; from Chapter 1
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

; remove elements recursively
(define rember*
  (lambda (a l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? (car l) a) (rember* a (cdr l)))
         (else (cons (car l) (rember* a (cdr l))))))
      (else (cons
             (rember* a (car l))
             (rember* a (cdr l)))))))

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons old (cons new (cdr l))))
         (else (cons (car l) (insertR* new old (cdr l))))))
      (else (cons
             (insertR* new old (car l))
             (insertR* new old (cdr l)))))))

; (insertR* 'roast 'chuck '((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood))

(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? (car l) a)
          (add1 (occur* a (cdr l))))
         (else (occur* a (cdr l)))))
      (else (plus
             (occur* a (car l))
             (occur* a (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? old (car l)) (cons new (subst* new old (cdr l))))
         (else (cons (car l) (subst* new old (cdr l))))))
       (else (cons
              (subst* new old (car l))
              (subst* new old (cdr l)))))))

;(subst* 'orange 'banana '((banana) (split ((((banana ice))) (cream (banana)) sherbet)) (banana) (bread) (banana brandy)))

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? (car l) old) (cons new l))
         (else (cons (car l) (insertL* new old (cdr l))))))
      (else (cons
             (insertL* new old (car l))
             (insertL* new old (cdr l)))))))

;(insertL* 'pecker 'chuck '((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood))

(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
       (or
         (eq? (car l) a)
         (member* a (cdr l))))
      (else (or
             (member* a (car l))
             (member* a (cdr l)))))))

;(member* 'chips '((potato) (chips ((with) fish) (chips))))

; The function "leftmost" finds the leftmost atom in a non-empty list of S-expressions.
; that does not contain the empty list
(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

;(define eqlist?
;  (lambda (l1 l2)
;    (cond
;      ((and (null? l1) (null? l2)) #t)
;      ((or (null? l1) (null? l2)) #f)
;      ((and (atom? (car l1)) (atom? (car l2)))
;       (and (eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
;      ((or (atom? (car l1)) (atom? (car l2))) #f)
;      (else (and
;             (eqlist? (car l1) (car l2))
;             (eqlist? (cdr l1) (cdr l2)))))))
      

;(eqlist? '(strawberry (ice) cream) '(strawberry (ice) cream))

; the first argument can be an atom or a list of S-expressions
; the second argument can be an atom or a list of S-expressions
(define equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2))
       (eqan? s1 s2))
      ((or (atom? s1) (atom? s2)) #f)
      (else (eqlist? s1 s2)))))

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or  (null? l1) (null? l2)) #f)
      (else
       (and (equal? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2)))))))

(define rember
  (lambda (s l)
    (cond
      ((null? l) (quote ()))
      ((equal? (car l) s) (cdr l))
      (else (cons (car l)
                  (rember s (cdr l)))))))