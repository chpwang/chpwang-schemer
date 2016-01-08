#lang scheme

(define member?
  (lambda (a lat)
    (cond ((null? lat) #f)
          (else
           (or (eq? a (car lat))
               (member? a (cdr lat)))))))

#|
我的版本，包含了不相邻（连续）的判断，但书里要求要相邻才 #t

(define two-in-a-row?
  (lambda (lat)
    (cond ((null? lat) #f)
          (else
           (or (member? (car lat) (cdr lat))
               (two-in-a-row? (cdr lat)))))))
|#

(define is-first?
  (lambda (a lat)
    (cond ((null? lat) #f)
          (else
           (eq? a (car lat))))))

(define two-in-a-row-b?
  (lambda (a lat)
    (cond ((null? lat) #f)
          (else
           (or (eq? a (car lat))
               (two-in-a-row-b? (car lat) (cdr lat)))))))

(define two-in-a-row?
  (lambda (lat)
    (cond ((null? lat) #f)
          (else
           (two-in-a-row-b? (car lat) (cdr lat))))))

;(two-in-a-row? '(Italian sardines spaghetti sardines parsley))

(define sum-of-prefixes-help
  (lambda (prefixes tup)
    (cond ((null? tup) '())
          (else
           (cons (+ prefixes (car tup))
                 (sum-of-prefixes-help (+ prefixes (car tup))
                                       (cdr tup)))))))

(define sum-of-prefixes
  (lambda (tup)
    (cond ((null? tup) '())
          (else
           (sum-of-prefixes-help 0 tup)))))



;(sum-of-prefixes '(2 1 9 17 0))

#|
;;;我写出 sum-of-prefixes 后突然想到然后写出的 SICP 的 2.18
;;;辅助函数的参数还可以减少一个（也就是去掉 i）
(define reverse-item-help
  (lambda (i l li)
    (cond ((null? li) (cons i l))
          (else
           (reverse-item-help (car li)
                              (cons i l)
                              (cdr li))))))

(define reverse-item
  (lambda (l)
    (cond ((null? l) '())
          (else
           (reverse-item-help (car l) '() (cdr l))))))

(reverse-item '(2 1 9 17 0))
|#

(define scramble
  (lambda (tup)
    (cond ((null? tup) '())
          (else
           (scramble-help '()
                          tup)))))

(define scramble-help
  (lambda (prefixes tup)
    (cond ((null? tup) '())
          (else
           (cons (pick (car tup)
                           (cons (car tup)
                                 prefixes))
                 (scramble-help (cons (car tup)
                                      prefixes)
                                (cdr tup)))))))

(define pick
  (lambda (n l)
    (cond ((zero? (- n 1)) (car l))
          (else
           (pick (- n 1) (cdr l))))))

(scramble '(1 2 3 1 2 3 4 1 8 2 10))




















