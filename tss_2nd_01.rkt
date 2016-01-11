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

(define length
  (lambda (l)
    (cond ((null? l) 0)
          (else
           (+ 1 (length (cdr l)))))))

(define eqan?
  (lambda (a1 a2)
    (cond ((and (number? a1) (number? a2))
           (= a1 a2))
          ((or (number? a1) (number? a2))
           #f)
          (else (eq? a1 a2)))))

(define eqlist?
  (lambda (l1 l2)
    (cond ((and (null? l1) (null? l2))
           #t)
          ((or (null? l1) (null? l2))
           #f)
          (else
           (and (equal? (car l1) (car l2))
                (eqlist? (cdr l1) (cdr l2)))))))

(define equal?
  (lambda (s1 s2)
    (cond ((and (atom? s1) (atom? s2))
           (eqan? s1 s2))
          ((or (atom? s1) (atom? s2))
           #f)
          (else
           (eqlist? s1 s2)))))



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
(define atom?
  (lambda (s)
    (and (not (pair? s)) (not (null? s)))))


#|
;;;;;; 我写的复杂版本 - 对比书上的第一版本好像还可以
(define rember1*
  (lambda (a l)
    (letrec ((R1* (lambda (ls)
                    (cond ((null? ls) '())
                          ((atom? (car ls))
                           (cond ((eq? (car ls) a)
                                  (cdr ls))
                                 (else
                                  (cons (car ls)
                                        (R1* (cdr ls))))))
                          (else
                           (cond ((= (L (car ls)) (L (R1* (car ls))))
                                  (cons (car ls)
                                        (R1* (cdr ls))))
                                 (else
                                  (cons (R1* (car ls))
                                        (cdr ls))))))))
             (L (lambda (s)
                  (cond ((null? s) 0)
                        ((atom? (car s))
                         (+ 1 (L (cdr s))))
                        (else
                         (+ (L (car s)) (L (cdr s))))))))
      (R1* l))))
|#

(define max
  (lambda (a b)
    (if (> a b) a b)))

(define depth*
  (lambda (l)
    (cond ((null? l) 1)
          ((atom? (car l))
           (depth* (cdr l)))
          (else
           (max
            (+ 1 (depth* (car l)))
            (depth* (cdr l)))))))

;(depth* '(() ((bitter butter) (makes) (batter (bitter))) butter))

(define scramble
  (lambda (tup)
    (letrec ((S (lambda (prefixes tup)
                  (cond ((null? tup) '())
                        (else
                         (let ((rp (cons (car tup)
                                         prefixes)))
                           (cons (pick (car tup)
                                       rp)
                                 (S rp
                                    (cdr tup)))))))))
      (S '() tup))))

;(scramble '(1 2 3 1 2 3 4 1 8 2 10))

#|
;;;;; old version of leftmost
(define leftmost
  (lambda (l)
    (cond ((null? l) '())
          ((atom? (car l))
           (car l))
          (else
           (let ((carl (leftmost (car l))))
             (cond ((atom? carl) carl)
                   (else
                    (leftmost (cdr l)))))))))
|#

#|
;;;;;; 我自己写的新版本
(define leftmost
  (lambda (l)
    (letcc hop
      (letrec ((le (lambda (l)
                     (cond ((null? l) '())
                           ((atom? (car l))
                            (hop (car l)))
                           (else
                            (le (car l))
                            (le (cdr l)))))))
        (le l)))))
|#

(define leftmost
  (lambda (l)
    (letcc hop
      (letrec ((le (lambda (l)
                     (cond ((null? l) '())
                           ((atom? (car l))
                            (hop (car l)))
                           (else
                            (le (car l))
                            (le (cdr l)))))))
        (le l)))))

;(leftmost '((() ((())) (() (ddc av))) b (c)))

#|
;;;;;; 老版本
(define rember1*
  (lambda (a l)
    (letrec ((R1* (lambda (ls)
                    (cond ((null? ls) '())
                          ((atom? (car ls))
                           (cond ((eq? (car ls) a)
                                  (cdr ls))
                                 (else
                                  (cons (car ls)
                                        (R1* (cdr ls))))))
                          (else
                           (let ((rcar (R1* (car ls))))
                             (cond ((eqlist? (car ls) rcar)
                                    (cons (car ls)
                                          (R1* (cdr ls))))
                                        (else
                                         (cons rcar
                                               (cdr ls))))))))))
      (R1* l))))
|#

#|
;;;;;;; 我写的版本 - 此解关键是洞察并构造出成功移除与否的返回值，成功为 list，失败为 atom
;;;;;;; 看了书上的版本后，反思觉得不要过早使用 letrec, let 等，先写出函数再化简
(define rember1*
  (lambda (a l)
    (letrec ((R1* (lambda (ls out)
                    (cond ((null? ls) (out #f))
                          ((atom? (car ls))
                           (cond ((eq? (car ls) a)
                                  (cdr ls))
                                 (else
                                  (cons (car ls)
                                        (R1* (cdr ls) out)))))
                          (else
                           (let ((cl (help (car ls))))
                             (if (atom? cl)
                                 (cons (car ls)
                                       (R1* (cdr ls) out))
                                 (cons cl
                                       (cdr ls))))))))
             (help (lambda (li)
                     (letcc hop
                       (R1* li hop)))))
      (help l))))

;(rember1* 'tomatoes '((pasta meat) pasta (noodles meat sauce) meat tomatoes))
;(rember1* 'salad '((Swedish rye) (French (mustard ssalad turkey)) (salad)))
;(rember1* 'salad '(((a)) b b))
|#

(define rm
  (lambda (a ls oh)
    (cond ((null? ls) (oh #f))
          ((atom? (car ls))
           (cond ((eq? (car ls) a)
                  (cdr ls))
                 (else
                  (cons (car ls)
                        (rm a (cdr ls) oh)))))
          (else
           (if (atom? (letcc out
                        (rm a (car ls) out)))
               (cons (car ls)
                     (rm a (cdr ls) oh))
               (cons (rm a (car ls) oh)
                     (cdr ls)))))))

(define rember1*
  (lambda (a l)
    (let ((new-l (letcc hop
                   (rm a l hop))))
      (if (atom? new-l)
          l
          new-l))))

(define rember1**
  (lambda (a l)
    (try hop (rm a l hop) l)))

(rember1** 'meat '((pasta meat) pasta (noodles (meat) sauce) meat tomatoes))
(rember1** 'salad '((Swedish rye) (French (mustard salad turkey)) (salad)))



;;;;;;; Chapter 15 - The Difference Between Men and Boys ... ;;;;;;;



















