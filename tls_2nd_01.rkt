#lang scheme
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (s-exp)
    (cond 
      ((null? s-exp) #t)
      ((atom? (car s-exp)) (lat? (cdr s-exp)))
      (else #f))))

(define member?
  (lambda (a lat)
    (cond ((null? lat) #f)
          (else
           (or (eq? a (car lat))
               (member? a (cdr lat)))))))

(define rember
  (lambda (a lat)
    (cond ((null? lat) '())
          ((eq? a (car lat))
           (cdr lat))
          (else (cons (car lat)
                      (rember a (cdr lat)))))))

(define firsts
  (lambda (l)
    (cond ((null? l) '())
          (else
           (cons (car (car l))
                 (firsts (cdr l)))))))


(firsts '((a b) (c d) (e f)))
