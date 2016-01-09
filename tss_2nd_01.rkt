#lang scheme

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

;;;;;;; Chapter 2 - Take Cover ;;;;;;;

(define multirember
  (letrec ((multirember (lambda (a lat)
                          (cond ((null? lat) '())
                                ((eq? a (car lat))
                                 (multirember a (cdr lat)))
                                (else
                                 (cons (car lat)
                                       (multirember a (cdr lat))))))))
    multirember))

(define multirember-f
  (lambda (test?)
    (letrec ((m-f (lambda (a lat)
                    (cond ((null? lat) '())
                          ((test? a (car lat))
                           (m-f a (cdr lat)))
                          (else (cons (car lat)
                                      (m-f a (cdr lat))))))))
      m-f)))

;(multirember 'pie '(apple custard pie linzer pie torte))

(define member?
  (lambda (a lat)
    (letrec ((m (lambda (l)
                  (cond ((null? l) #f)
                        (else
                         (or (eq? a (car l))
                             (m (cdr l))))))))
      (m lat))))

;(member? 'ice '(salad greens with pears ice brie chesse frozen yogurt))

(define union
  (lambda (set1 set2)
    (letrec ((u (lambda (s)
                  (cond ((null? s) set2)
                        ((M? (car s) set2)
                         (u (cdr s)))
                        (else
                         (cons (car s)
                               (u (cdr s)))))))
             (M? (lambda (a lat)
                   (letrec ((m (lambda (l)
                                 (cond ((null? l) #f)
                                       (else
                                        (or (eq? a (car l))
                                            (m (cdr l))))))))
                     (m lat)))))
      (u set1))))

;(union '(tomatoes and macaroni tt) '(macaroni and cheese))

(define two-in-a-row?
  (letrec ((help (lambda (a lat)
                   (cond ((null? lat) #f)
                         (else
                          (or (eq? a (car lat))
                              (help (car lat) (cdr lat))))))))
    (lambda (lat)
      (cond ((null? lat) #f)
            (else
             (help (car lat) (cdr lat)))))))

;(two-in-a-row? '(Italian sardines spaghetti sardines parsley))


(define sum-of-prefixes
  (letrec ((help (lambda (prefixes tup)
                   (cond ((null? tup) '())
                         (else
                          (cons (+ prefixes (car tup))
                                (help (+ prefixes (car tup))
                                      (cdr tup))))))))
    (lambda (tup)
      (help 0 tup))))

;(sum-of-prefixes '(2 1 9 17 0))

(define pick
  (lambda (n l)
    (cond ((zero? (- n 1)) (car l))
          (else
           (pick (- n 1) (cdr l))))))

(define scramble
  (lambda (tup)
    (letrec ((S (lambda (prefixes tup)
                  (cond ((null? tup) '())
                        (else
                         (cons (pick (car tup)
                                  (cons (car tup)
                                        prefixes))
                               (S (cons (car tup)
                                        prefixes)
                                  (cdr tup))))))))
      (S '() tup))))

(scramble '(1 2 3 1 2 3 4 1 8 2 10))


;;;;;;; Chapter 13 - Hop, Skip, and Jump ;;;;;;;

























