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


(define insertR
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? old (car lat))
           (cons old
                 (cons new
                       (cdr lat))))
          (else (cons (car lat)
                      (insertR new old (cdr lat)))))))

(define insertL
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? old (car lat))
           (cons new
                 lat))
          (else (cons (car lat)
                      (insertL new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? old (car lat))
           (cons new
                 (cdr lat)))
          (else (cons (car lat)
                      (subst new old (cdr lat)))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond ((null? lat) '())
          ((or (eq? o1 (car lat))
               (eq? o2 (car lat)))
           (cons new
                 (cdr lat)))
          (else (cons (car lat)
                      (subst2 new o1 o2 (cdr lat)))))))


(define multirember
  (lambda (a lat)
    (cond ((null? lat) '())
          ((eq? a (car lat))
           (multirember a (cdr lat)))
          (else (cons (car lat)
                      (multirember a (cdr lat)))))))


(define multiinsertR
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? old (car lat))
           (cons old
                 (cons new
                       (multiinsertR new old (cdr lat)))))
          (else (cons (car lat)
                      (multiinsertR new old (cdr lat)))))))


(define multiinsertL
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? old (car lat))
           (cons new
                 (cons old
                       (multiinsertL new old (cdr lat)))))
          (else (cons (car lat)
                      (multiinsertL new old (cdr lat)))))))

(define multisubst
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? old (car lat))
           (cons new
                 (multisubst new old (cdr lat))))
          (else (cons (car lat)
                      (multisubst new old (cdr lat)))))))

(define add1
  (lambda (n)
    (+ n 1)))



















