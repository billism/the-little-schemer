#lang scheme

(require "ch05.scm")
(provide (all-defined-out))

; from Chapter 1
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

; from Chapter 3
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) a) (multirember a (cdr lat)))
      (else (cons (car lat)
                  (multirember a (cdr lat)))))))

; rewrite member? by using equal? instead of eq?
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or
             (equal? (car lat) a)
             (member? a (cdr lat)))))))

; check if a list of atoms is a set
(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))

; using member?
;(define makeset
;  (lambda (lat)
;    (cond
;      ((null? lat) (quote ()))
;      ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
;      (else (cons
;             (car lat)
;             (makeset (cdr lat)))))))

; using multirember
(define makeset
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else (cons
             (car lat)
             (makeset (multirember (car lat) (cdr lat))))))))

(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      ((member? (car set1) set2) (subset? (cdr set1) set2))
      (else #f))))

(define eqset?
  (lambda (set1 set2)
    (and ((subset? set1 set2) (subset? set2 set1)))))

(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      ((member? (car set1) set2) #t)
      (else (intersect? (cdr set1 set2))))))

(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) (quote ()))
      ((member? (car set1) set2)
       (cons (car set1) (intersect (cdr set1) set2)))
      (else
       (intersect (cdr set1) set2)))))

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2) (union (cdr set1) set2))
      (else
       (cons
        (car set1)
        (union (cdr set1) set2))))))

(define diff
  (lambda (set1 set2)
    (cond
      ((null? set1) (quote ()))
      ((member? (car set1) set2) (diff (cdr set1) set2))
      (else (cons (car set1)
                  (diff (cdr set1) set2))))))

; intersection of a list of sets
; assuming the list is non-empty
(define intersectall
  (lambda (l-set)
    (cond
      ((null? (car l-set)) (quote ()))
      (else (intersect (car l-set)
                       (intersectall (cdr l-set)))))))

; let's talk about pairs
; a pair is just a list with only two atoms.
(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x) #f))
      ((null? (cdr (cdr x))) #t)
      (else #f))))

(define first
  (lambda (p)
    (cond
      (else (car p)))))

(define second
  (lambda (p)
    (cond
      (else (car (cdr p))))))

(define third
  (lambda (l)
    (car (cdr (cdr l)))))

(define build
  (lambda (s1 s2)
    (cond
      (else (cons s1
                  (cons s2 (quote ())))))))

(define firsts
  (lambda (lp)
    (cond
      ((null? lp) (quote ()))
      (else
       (cons
        (car (car lp))
        (firsts (cdr lp)))))))

(define seconds
  (lambda (lp)
    (cond
      ((null? lp) (quote ()))
      (else
       (cons
        (car (cdr (car lp)))
        (seconds (cdr lp)))))))

; fun is a type of binary relationship
(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define revpair
  (lambda (pair)
    (build
     (second pair)
     (first pair))))

; reverse mapping
(define revrel
  (lambda (rel)
    (cond
      ((null? rel) (quote ()))
      (else (cons
             (revpair (car rel))
             (revrel (cdr rel)))))))

(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))

(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))))
