#lang scheme
(define-syntax letcc 
  (syntax-rules () 
    ((letcc var body ...) 
     (call-with-current-continuation 
       (lambda (var)  body ... )))))

(define-syntax try 
  (syntax-rules () 
    ((try var a . b) 
     (letcc success 
       (letcc var (success a)) . b))))

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

;(scramble '(1 2 3 1 2 3 4 1 8 2 10))


;;;;;;; Chapter 13 - Hop, Skip, and Jump ;;;;;;;

(define intersect
  (lambda (set1 set2)
    (letrec ((I (lambda (set)
                  (cond ((null? set) '())
                        ((member? (car set) set2)
                         (cons (car set)
                               (I (cdr set))))
                        (else
                         (I (cdr set)))))))
      (cond ((null? set2) '())
            (else
             (I set1))))))

(define intersectall
  (lambda (l-sets)
    (letcc hop
      (letrec ((A (lambda (lsets)
                       (cond ((null? (car lsets))
                              (hop '()))
                             ((null? (cdr lsets))
                              (car lsets))
                             (else
                              (I (car lsets)
                                 (A (cdr lsets)))))))
               (I (lambda (set1 set2)
                    (letrec ((J (lambda (set)
                                  (cond ((null? set) '())
                                        ((member? (car set) set2)
                                         (cons (car set)
                                               (J (cdr set))))
                                        (else
                                         (J (cdr set)))))))
                      (cond ((null? set2) (hop '()))
                            (else
                             (J set1)))))))
        (cond  ((null? l-sets) '())
               (else
                (A l-sets)))))))

;(intersectall '((tomatoes 3 and macaroni t2t) (2 5 6) (macaroni and 3 cheese)))

(define rember
  (lambda (a lat)
    (letcc hop
      (letrec ((R (lambda (l)
                    (cond ((null? l) (hop lat))
                          ((eq? a (car l))
                           (cdr l))
                          (else
                           (cons (car l)
                                 (R (cdr l))))))))
        (R lat)))))

(define rember-beyond-first
  (lambda (a lat)
    (letcc hop
      (letrec ((R (lambda (l)
                    (cond ((null? l) (hop lat))
                          ((eq? a (car l))
                           '())
                          (else
                           (cons (car l)
                                 (R (cdr l))))))))
        (R lat)))))

(define rember-upto-last
  (lambda (a lat)
    (letcc skip
      (letrec ((R (lambda (l)
                    (cond ((null? l) '())
                          ((eq? a (car l))
                           (skip (R (cdr l))))
                          (else
                           (cons (car l)
                                 (R (cdr l))))))))
        (R lat)))))

;(rember-upto-last 'roots '(noodles spaghetti roots spatzle bean-thread roots potatoes yam others rice))

;;;;;;; Chapter 14 - Let There Be Names ;;;;;;;
















