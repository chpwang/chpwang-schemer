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
#|
(define rember1*
  (lambda (a l)
    (let ((new-l (letcc hop
                   (rm a l hop))))
      (if (atom? new-l)
          l
          new-l))))
|#

(define rember1**
  (lambda (a l)
    (try hop (rm a l hop) l)))

;(rember1** 'meat '((pasta meat) pasta (noodles (meat) sauce) meat tomatoes))
;(rember1** 'salad '((Swedish rye) (French (mustard salad turkey)) (salad)))



;;;;;;; Chapter 15 - The Difference Between Men and Boys ... ;;;;;;;
#|
(define x '())

(define gourmand
  (lambda (food)
    (set! x food)
    (cons food
          (cons x '()))))

(define dinerR
  (lambda (food)
    (set! x food)
    (cons 'milkshake
          (cons food
                '()))))
|#


(define omnivore
  (let ((x (quote minestrone)))
    (lambda (food)
      (set! x food)
      (cons food
            (cons x
                  '())))))


;;;;;;; Chapter 16 - Ready, Set, Bang! ;;;;;;;

(define sweet-tooth
  (lambda (food)
    (cons food
          (cons 'cake '()))))

(define last 'angelfood)

(define sweet-toothL
  (lambda (food)
    (set! last food)
    (cons food
          (cons 'cake '()))))

(define ingredients '())

(define sweet-toothR
  (lambda (food)
    (set! ingredients (cons food ingredients))
    (cons food
          (cons 'cake '()))))


(define Y
  (lambda (y)
    ((lambda (mk-length)
       (mk-length mk-length))
     (lambda (mk-length)
       (y
        (lambda (x)
          ((mk-length mk-length) x)))))))

(define L
  (lambda (length)
    (lambda (l)
      (cond ((null? l) 0)
            (else (+ 1 (length (cdr l))))))))


(define Y!
  (lambda (fun)
    (let ((h (lambda (l) 0)))
      (set! h
            (fun (lambda (x) (h x))))
      h)))

(define Y-bang
  (lambda (f)
    (letrec ((h (f (lambda (arg) (h arg)))))
      h)))

(define biz
  (let ((x 0))
    (lambda (f)
      (set! x (+ 1 x))
      (lambda (a)
        (if (= a x)
            0
            (f a))))))

;((Y! biz) 1)


;;;;;;; Chapter 17 - We Change, Therefor We Are ! ;;;;;;;


(define find
  (lambda (n ns rs)
    (letrec ((help
              (lambda (ln lr)
                (cond ((null? ln) #f)
                      ((= n (car ln))
                       (car lr))
                      (else
                       (help (cdr ln) (cdr lr)))))))
      (help ns rs))))

(define deepM
  (let ((Ns '())
        (Rs '()))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result (if (zero? n)
                              'pizza
                              (consC (deepM (- n 1))
                                    '()))))
              (set! Ns (cons n Ns))
              (set! Rs (cons result Rs))
              result)
            exists)))))

(define counter '())
(define set-counter '())

(define consC
  (let ((count 0))
    (set! counter (lambda () count))
    (set! set-counter (lambda (x) (set! count x)))
    (lambda (s l)
      (set! count (+ 1 count))
      (cons s l))))


#|
;;;;;;我写的版本和对应的 deepM - 我把 deepM 里三个地方都换成了 consC
(define supercounter
  (lambda (f)
    (letrec ((S (lambda (n)
                  (cond ((zero? n) (counter))
                        (else
                         (f n)
                         (S (- n 1)))))))
          (S 1000))))

(define deepM
  (let ((Ns '())
        (Rs '()))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result (if (zero? n)
                              'pizza
                              (consC (deepM (- n 1))
                                    '()))))
              (set! Ns (consC n Ns))
              (set! Rs (consC result Rs))
              result)
            exists)))))
|#

(define supercounter
  (lambda (f)
    (letrec ((S (lambda (n)
                  (cond ((zero? n) (f n))
                        (else
                         (f n)
                         (S (- n 1)))))))
      (S 1000)
      (counter))))


(define rember1*C
  (lambda (a l)
    (letrec ((R (lambda (ls oh)
                   (cond ((null? ls) (oh #f))
                         ((atom? (car ls))
                          (cond ((eq? (car ls) a)
                                 (cdr ls))
                                (else
                                 (consC (car ls)
                                        (R(cdr ls) oh)))))
                         (else
                          (if (atom? (letcc out
                                       (R (car ls) out)))
                              (consC (car ls)
                                     (R (cdr ls) oh))
                              (consC (R (car ls) oh)
                                     (cdr ls))))))))
      (let ((new-l (letcc hop
                     (rm a l hop))))
        (if (atom? new-l)
            l
            new-l)))))


;(rember1*C 'noodles '((food) more (food)))


;;;;;;; Chapter 18 - We Change, Therefore We Are The Same! ;;;;;;;

(define lots
  (lambda (n)
    (cond ((zero? n) '())
          (else
           (konsC 'egg
                 (lots (- n 1)))))))

(define lenkth
  (lambda (l)
    (cond ((null? l) 0)
          (else
           (+ 1 (lenkth (kdr l)))))))

(define add-at-end
  (lambda (l)
    (cond ((null? (kdr l))
           (konsC (kar l)
                  (konsC 'egg
                        '())))
          (else
           (konsC (kar l)
                 (add-at-end (kdr l)))))))

(define kounter '())
(define set-kounter '())

(define konsC
  (let ((kount 0))
    (set! kounter (lambda () kount))
    (set! set-kounter (lambda (x) (set! kount x)))
    (lambda (a b)
      (set! kount (+ 1 kount))
      (kons a b))))

(define add-at-end-too
  (lambda (l)
    (letrec ((A (lambda (ls)
                  (cond ((null? (kdr ls))
                         (set-kdr ls
                                  (konsC 'egg
                                        '())))
                        (else
                         (A (kdr ls)))))))
      (A l)
      l)))

#|
(define kons
  (lambda (kar kdr)
    (lambda (selector)
      (selector kar kdr))))

(define kar
  (lambda (k)
    (k (lambda (a b) a))))

(define kdr
  (lambda (k)
    (k (lambda (a b) b))))
|#

(define bons
  (lambda (kar)
    (let ((kdr '()))
      (lambda (selector)
        (selector
         (lambda (x) (set! kdr x))
         kar
         kdr)))))

(define kar
  (lambda (bons)
    (bons (lambda (s a b) a))))

(define kdr
  (lambda (bs)
    (bs (lambda (s a b) b))))

(define set-kdr
  (lambda (bs x)
    ((bs (lambda (s a b) s)) x)))

(define kons
  (lambda (kar kdr)
    (let ((B (bons kar)))
      (set-kdr B kdr)
      B)))

(define eklist?
  (lambda (ls1 ls2)
    (cond ((null? ls1) (null? ls2))
          ((null? ls2) #f)
          (else
           (and (eq? (kar ls1) (kar ls2))
                (eklist? (kdr ls1) (kdr ls2)))))))

(define same?
  (lambda (kons1 kons2)
    (let ((t1 (kdr kons1))
          (t2 (kdr kons2)))
      (set-kdr kons1 '1)
      (set-kdr kons2 '2)
      (let ((v (= (kdr kons1) (kdr kons2))))
        (set-kdr kons1 t1)
        (set-kdr kons2 t2)
        v))))

(define dozen (lots 12))

(define bakers-dozen (add-at-end dozen))

(define bakers-dozen-too (add-at-end-too dozen))

(define bakers-dozen-again (add-at-end dozen))

(define last-kons
  (lambda (ls)
    (cond ((null? (kdr ls)) ls)
          (else
           (last-kons (kdr ls))))))

(define long (lots 12))

(define finite-lenkth
  (lambda (p)
    (letcc infinite
      (letrec ((C (lambda (p q)
                    (cond ((same? p q) (infinite #f))
                          ((null? q) 0)
                          ((null? (kdr q)) 1)
                          (else
                           (+ (C (sl p) (qk q))
                              2)))))
               (qk (lambda (x) (kdr (kdr x))))
               (sl (lambda (x) (kdr x))))
        (cond ((null? p) 0)
              (else
               (+ 1 (C p (kdr p)))))))))


;;;;;;; Chapter 19 - Absconding with the Jewels ;;;;;;;

(define deep
  (lambda (n)
    (if (zero? n)
        'pizza
        (cons (deep (- n 1))
               '()))))

#|
;;;;;;; 我的失败版本 - 在还没理解 set! 的机制的情况下
;;;;;;; 书上采用的是用 letcc 中断一个正在进行的函数，并把中断带出
;;;;;;; 以便设置好参数后从中断处继续
(define set-innermo '())
(define see-innermo '())

(define deepS
  (let ((inner 1))
    (set! set-innermo (lambda (x) (set! inner x)))
    (set! see-innermo (lambda () inner))
    (lambda (num p)
      (letrec ((D (lambda (n)
                    (cond ((zero? n) inner)
                          (else
                           (cons (D (- n 1))
                                 '()))))))
        (set! inner p)
        (D num)))))

(define test (deepS 5 'neapolitan))
|#


(define toppings '())

#|
;;; 我的版本 - 看了书中提示用 letcc 后盲写的
(define deepB
  (lambda (num)
    (letcc pause
      (letrec ((B (lambda (m)
                    (cond ((zero? m)
                           (let ((inner '()))
                             (letcc continue
                               (pause
                                (set! toppings
                                      (lambda (x) (continue (set! inner x))))))
                             inner))
                          (else
                           (cons (B (- m 1))
                                 '()))))))
        (B num)))))
|#

#|
;;; 看了书上的答案后改进的版本。书上的简单多了。
;;; 书上用 deepB 是直接输出一个答案并且保存中断，而我上面的版本则
;;; 在应用 deepB 后不输出，用 toppings 来输出

(define deepB
  (lambda (m)
    (cond ((zero? m)
           (let ((inner 'pizza))
             (letcc continue
               (set! toppings
                     (lambda (x) (continue (set! inner x)))))
             inner))
          (else
           (cons (deepB (- m 1))
                 '())))))
|#

;;;; 书上连 lambda 和 let 都不用，直接把 continue 当赋值给 toppings
(define deepB
  (lambda (m)
    (cond ((zero? m)
           (letcc continue
             (set! toppings continue)
             'pizza))
          (else
           (cons (deepB (- m 1))
                 '())))))

(define deep&co
  (lambda (m k)
    (cond ((zero? m) (k 'pizza))
          (else
           (deep&co (- m 1)
                    (lambda (x)
                      (k (cons x '()))))))))

(define deep&coB
  (lambda (m k)
    (cond ((zero? m)
           (let ()
             (set! toppings k)
             (k 'pizza)))
          (else
           (deep&coB (- m 1)
                    (lambda (x)
                      (k (cons x '()))))))))

(define two-in-a-row?
  (letrec ((help (lambda (a lat)
                   (cond ((null? lat) #f)
                         (else
                          (let ((nxt (car lat)))
                            (or (eq? a nxt)
                                (help nxt (cdr lat)))))))))
    (lambda (lat)
      (cond ((null? lat) #f)
            (else
             (help (car lat) (cdr lat)))))))

;(two-in-a-row? '(Italian sardines spaghetti sardines parsley))




;(start-it2 '((potato) (chips (chips (with))) fish))

#|
;;;; 我的版本
;;;; 注意，如果函数的参数 list 里没有 atom
;;;; waddle 函数就不会给 fill 赋值，会出错
;;;; 故 letrec 的 value part 里不能用 null？
;;;; 判断 ls 后就直接 (H (get-first ls))

(define two-in-a-row*?
  (lambda (ls)
    (letrec ((H (lambda (a)
                  (let ((b (get-next 'go)))
                    (cond ((null? b) #f)
                          (else
                           (or (eq? a b)
                               (H b))))))))
      (let ((fst (get-first ls)))
        (if (atom? fst)
            (H fst)
            #f)))))
|#


(define two-in-a-row*?
  (lambda (ls)
    (letrec ((H (lambda (a)
                  (let ((b (get-next 'go)))
                    (cond ((null? b) #f)
                          (else
                           (or (eq? a b)
                               (H b)))))))
             (get-next (lambda (x)
                         (letcc here-again
                           (set! leave here-again)
                           (fill 'go))))
             (waddle (lambda (l)
                       (cond ((null? l) '())
                             ((atom? (car l))
                              (let ()
                                (letcc back
                                  (set! fill back)
                                  (leave (car l)))
                                (waddle (cdr l))))
                             (else
                              (let ()
                                (waddle (car l))
                                (waddle (cdr l)))))))
             (fill (lambda (x) x))
             (leave (lambda (x) x)))
      (let ((fst (letcc here
                   (set! leave here)
                   (waddle ls)
                   (leave '()))))
        (if (atom? fst)
            (H fst)
            #f)))))




;(two-in-a-row*? '((mozzarella) (cake) mozzarella))
;(two-in-a-row*? '((potato) (chips ((with) fish) (fish))))
;(two-in-a-row*? '((potato) (chips ((with) fish) (chips))))
;(two-in-a-row*? '(() ((() ) ())))
;(two-in-a-row*? '(((food) ()) (((food)))))


;;;;;;; Chapter 20 - What's in Store ? ;;;;;;;






















