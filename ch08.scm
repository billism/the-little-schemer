#lang scheme

(require "ch04.scm")
(require "ch06.scm")
(provide (all-defined-out))

;(define rember_f
;  (lambda (test? a l)
;    (cond
;      ((null? l) (quote ()))
;      ((test? (car l) a) (cdr l))
;      (else (cons (car l)
;                  (rember_f test? a (cdr l)))))))

; a higher-order function
; return a function
(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

; a higher-order function
; take a function
; return a function
(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) (quote ()))
        ((test? (car l) a) (cdr l))
        (else (cons
               (car l)
               ((rember-f test?) a (cdr l))))))))

; ((rember-f eq?) 'tuna '(tuna salad is good))

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) (quote ()))
        ((test? (car l) old) (cons new (cons old (cdr l))))
        (else (cons (car l)
                    ((insertL-f test?) new old (cdr l))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) (quote ()))
        ((test? (car l) old) (cons old (cons new (cdr l))))
        (else (cons (car l)
                    ((insertR-f test?) new old (cdr l))))))))

(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        ((null? l) (quote ()))
        ((eq? (car l) old)
         (seq new old (cdr l)))
        (else (cons (car l)
                    ((insert-g seq) new old (cdr l))))))))

;recall
(define subst
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((eq? (car l) old) (cons new (cdr l)))
      (else (cons (car l)
                  (subst new old (cdr l)))))))

(define seqS
  (lambda (new old l)
    (cons new l)))

;(define insertL (insert-g seqL))
;(define insertR (insert-g seqR))
;(define subst (insert-g seqS))

(define seqrem
  (lambda (new old l)
    l))

(define rember
  (lambda (a l)
    ((insert-g seqrem) #f a l)))

;recall
;for representation (+ 2 3)
;(define value
;  (lambda (nexp)
;    (cond
;      ((atom? nexp) nexp)
;      ((eq? (operator nexp) (quote +))
;       (plus (value (1st-sub-exp nexp))
;             (value (2nd-sub-exp nexp))))
;      ((eq? (operator nexp) (quote *))
;       (times (value (1st-sub-exp nexp))
;              (value (2nd-sub-exp nexp))))
;      (else
;       (pow (value (1st-sub-exp nexp))
;            (value (2nd-sub-exp nexp)))))))

(define atom-to-function
  (lambda (x)
    (cond
      ((eq? x (quote +)) plus)
      ((eq? x (quote *)) times)
      (else pow))))

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else
       ((atom-to-function (operator nexp))
        (value (1st-sub-exp nexp))
        (value (2nd-sub-exp nexp)))))))

;recall
;(define multirember
;  (lambda (a lat)
;    (cond
;      ((null? lat) (quote ()))
;      ((eq? (car lat) a)
;       (multirember a (cdr lat)))
;      (else
;       (cons (car lat)
;             (multirember a (cdr lat)))))))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) (quote ()))
        ((test? (car lat) a)
         ((multirember-f test?) a (cdr lat)))
        (else
         (cons (car lat)
               ((multirember-f test?) a (cdr lat))))))))

; before: test? is a binary function
; below: test? is a unary function
(define multiremberT
  (lambda (test? lat)
    (cond
      ((null? lat) (quote ()))
      ((test? (car lat))
       (multiremberT test? (cdr lat)))
      (else
       (cons (car lat)
             (multiremberT test? (cdr lat)))))))

(define multirember&co
  (lambda (a lat col)
    (cond
      ((null? lat)
       (col (quote ()) (quote ())))
      ((eq? (car lat) a)
       (multirember&co a
                       (cdr lat)
                       (lambda (newlat seen)
                         (col newlat
                              (cons (car lat) seen)))))
      (else
       (multirember&co a
                       (cdr lat)
                       (lambda (newlat seen)
                         (col (cons (car lat) newlat)
                              seen)))))))

; a recursive function (but not tail-recursive)
; split the "lat" into two lists
(define test
  (lambda (a lat)
    (cond
      ((null? lat)
       (list (quote ()) (quote ())))
      ((eq? (car lat) a)
       (list
        (car (test a (cdr lat)))
        (cons (car lat) (car (cdr (test a (cdr lat)))))))
      (else
       (list
        (cons (car lat) (car (test a (cdr lat))))
        (car (cdr (test a (cdr lat)))))))))

; use "let*" local binding for more readable code
(define test2
  (lambda (a lat)
    (cond
      ((null? lat)
       (list (quote ()) (quote ())))
      (else
       (let*
           ([rst (test a (cdr lat))]
            [fst (car rst)]
            [snd (car (cdr rst))])
         (cond
           ((eq? (car lat) a)
            (list
             fst
             (cons (car lat) snd)))
           (else
            (list
             (cons (car lat) fst)
             snd))))))))

; rewrite the above code as a tail-recursive function
(define test3
  (lambda (a lat acc)
    (cond
      ((null? lat) acc)
      (else
       (let*
           ([fst (car acc)]
            [snd (car (cdr acc))])
         (cond
           ((eq? (car lat) a)
            (test3 a
                   (cdr lat)
                   (list
                    fst
                    (cons (car lat) snd))))
           (else
            (test3 a
                   (cdr lat)
                   (list
                    (cons (car lat) fst)
                    snd)))))))))

;(test 'a '(a b c a d e x g e a c m j a u i))
;(test2 'a '(a b c a d e x g e a c m j a u i))
;(test3 'a '(a b c a d e x g e a c m j a u i) '(() ()))

(define a-friend
  (lambda (x y)
    (null? y)))

;(multirember&co 'tuna '(and tuna) a-friend)

;recall
(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) oldL)
       (cons new
             (cons oldL
                   (multiinsertLR new oldL oldR (cdr lat)))))
      ((eq? (car lat) oldR)
       (cons oldR
             (cons oldR
                   (multiinsertLR new oldL oldR (cdr lat)))))
      (else
       (cons (car lat)
             (multiinsertLR new oldL oldR (cdr lat)))))))

(define add1
  (lambda (x)
    (+ x 1)))

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat)
       (col (quote ()) 0 0))
      ((eq? (car lat) oldL)
       (multiinsertLR&co new
                         oldL
                         oldR
                         (cdr lat)
                         (lambda (newlat L R)
                           (col (cons new (cons oldL newlat))
                                (add1 L)
                                R))))
      ((eq? (car lat) oldR)
       (multiinsertLR&co new
                         oldL
                         oldR
                         (cdr lat)
                         (lambda (newlat L R)
                           (col (cons oldR (cons new newlat))
                                L
                                (add1 R)))))
      (else
       (multiinsertLR&co new
                         oldL
                         oldR
                         (cdr lat)
                         (lambda (newlat L R)
                           (col (cons (car lat) newlat)
                                L
                                R)))))))

;(multiinsertLR&co 'salty 'fish 'chips '(chips and fish or fish and chips) list)

(define even?
  (lambda (n)
    (= (* (div n 2) 2) n)))

(define evens-only*
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((even? (car l))
          (cons (car l)
                (evens-only* (cdr l))))
         (else
          (evens-only* (cdr l)))))
      (else
       (cons (evens-only* (car l))
             (evens-only* (cdr l)))))))

; (evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2))

(define evens-only*&co
  (lambda (l col)
    (cond
      ((null? l)
       (col (quote ()) 1 0))
      ((atom? (car l))
       (cond
         ((even? (car l))
          (evens-only*&co (cdr l)
                          (lambda (newl p s)
                            (col (cons (car l) newl)
                                 (* (car l) p)
                                 s))))
         (else
          (evens-only*&co (cdr l)
                          (lambda (newl p s)
                            (col newl
                                 p
                                 (+ s (car l))))))))
      (else
       (evens-only*&co (car l)
                       (lambda (al ap as)
                         (evens-only*&co (cdr l)
                                         (lambda (dl dp ds)
                                           (col (cons al dl)
                                                (* ap dp)
                                                (+ as ds))))))))))

(define the-last-friend
  (lambda (newl product sum)
    (cons sum
          (cons product
                newl))))

;(evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) the-last-friend)