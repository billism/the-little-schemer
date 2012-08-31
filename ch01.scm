#lang scheme

(provide atom?)

; Chapter 1
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

