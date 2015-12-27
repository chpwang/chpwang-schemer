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
           (or (equal? a (car lat))
               (member? a (cdr lat)))))))

(define firsts
  (lambda (l)
    (cond ((null? l) '())
          (else
           (cons (car (car l))
                 (firsts (cdr l)))))))


(define insertR
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((equal? old (car lat))
           (cons old
                 (cons new
                       (cdr lat))))
          (else (cons (car lat)
                      (insertR new old (cdr lat)))))))

(define insertL
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((equal? old (car lat))
           (cons new
                 lat))
          (else (cons (car lat)
                      (insertL new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((equal? old (car lat))
           (cons new
                 (cdr lat)))
          (else (cons (car lat)
                      (subst new old (cdr lat)))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond ((null? lat) '())
          ((or (equal? o1 (car lat))
               (equal? o2 (car lat)))
           (cons new
                 (cdr lat)))
          (else (cons (car lat)
                      (subst2 new o1 o2 (cdr lat)))))))


(define multirember
  (lambda (a lat)
    (cond ((null? lat) '())
          ((equal? a (car lat))
           (multirember a (cdr lat)))
          (else (cons (car lat)
                      (multirember a (cdr lat)))))))


(define multisubst
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((equal? old (car lat))
           (cons new
                 (multisubst new old (cdr lat))))
          (else (cons (car lat)
                      (multisubst new old (cdr lat)))))))

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define addtup
  (lambda (tup)
    (cond ((null? tup) 0)
          (else
           (+ (car tup)
              (addtup (cdr tup)))))))

(define tup+
  (lambda (tup1 tup2)
    (cond ((null? tup1) tup2)
          ((null? tup2) tup1)
          (else
           (cons (+ (car tup1) (car tup2))
                 (tup+ (cdr tup1)
                       (cdr tup2)))))))

(define gthan
  (lambda (n m)
    (cond ((zero? n) #f)
          ((zero? m) #t)
          (else
           (gthan (sub1 n) (sub1 m))))))

(define lthan
  (lambda (n m)
    (cond ((zero? m) #f)
          ((zero? n) #t)
          (else
           (lthan (sub1 n) (sub1 m))))))

(define eqto
  (lambda (n m)
    (cond ((gthan n m) #f)
          ((lthan n m) #f)
          (else
           #t))))

(define expo
  (lambda (n m)
    (cond ((zero? m) 1)
          (else
           (* n (expo n (sub1 m)))))))

(define pick
  (lambda (n lat)
    (cond ((zero? (sub1 n)) (car lat))
          (else (pick (sub1 n)
                      (cdr lat))))))

(define no-nums
  (lambda (lat)
    (cond ((null? lat) '())
          ((number? (car lat))
           (no-nums (cdr lat)))
          (else (cons (car lat)
                      (no-nums (cdr lat)))))))

(define all-nums
  (lambda (lat)
    (cond ((null? lat) '())
          ((number? (car lat))
           (cons (car lat)
                 (all-nums (cdr lat))))
          (else (all-nums (cdr lat))))))

(define eqan?
  (lambda (a1 a2)
    (cond ((and (number? a1) (number? a2))
           (= a1 a2))
          ((or (number? a1) (number? a2))
           #f)
          (else (eq? a1 a2)))))

(define occur
  (lambda (a lat)
    (cond ((null? lat) 0)
          ((eqan? a (car lat))
           (add1 (occur a (cdr lat))))
          (else
           (occur a (cdr lat))))))

(define one?
  (lambda (n)
    (= n 1)))

(define rempick
  (lambda (n lat)
    (cond ((one? n) (cdr lat))
          (else (cons (car lat)
                      (rempick (sub1 n)
                               (cdr lat)))))))


(define rember*
  (lambda (a l)
    (cond ((null? l) '())
          ((atom? (car l))
           (cond ((eqan? a (car l))
                  (rember* a (cdr l)))
                 (else (cons (car l)
                             (rember* a (cdr l))))))
          (else (cons (rember* a (car l))
                      (rember* a (cdr l)))))))

#|
(rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))
(rember* 'sauce '(((tomato sauce))
                  ((bean) sauce)
                  (and ((flying)) sauce)))
|#

(define insertR*
  (lambda (new old l)
    (cond ((null? l) '())
          ((atom? (car l))
           (cond ((eqan? old (car l))
                  (cons old
                        (cons new
                              (insertR* new old (cdr l)))))
                 (else (cons (car l)
                             (insertR* new old (cdr l))))))
          (else (cons (insertR* new old (car l))
                      (insertR* new old (cdr l)))))))

#|
(insertR* 'roast 'chuck '((how much (wood))
                          could
                          ((a (wood) chuck))
                          (((chuck)))
                          (if (a) ((wood chuck)))
                          could chuck wood))
|#

(define occur*
  (lambda (a l)
    (cond ((null? l) 0)
          ((atom? (car l))
           (cond ((eqan? a (car l))
                  (add1 (occur* a (cdr l))))
                 (else
                  (occur* a (cdr l)))))
          (else
           (+ (occur* a (car l))
              (occur* a (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond ((null? l) '())
          ((atom? (car l))
           (cond ((eqan? old (car l))
                  (cons new
                        (subst* new old (cdr l))))
                 (else
                  (cons (car l)
                        (subst* new old (cdr l))))))
          (else (cons (subst* new old (car l))
                      (subst* new old (cdr l)))))))

#|
(subst* 'orange 'banana '((banana)
                          (split ((((banana ice)))
                                  (cream (banana))
                                  sherbet))
                          (banana)
                          (bread)
                          (banana brandy)))
|#

(define insertL*
  (lambda (new old l)
    (cond ((null? l) '())
          ((atom? (car l))
           (cond ((eqan? old (car l))
                  (cons new
                        (cons old
                              (insertL* new old (cdr l)))))
                 (else (cons (car l)
                             (insertL* new old (cdr l))))))
          (else (cons (insertL* new old (car l))
                      (insertL* new old (cdr l)))))))

#|
(insertL* 'pecker 'chuck '((how much (wood))
                          could
                          ((a (wood) chuck))
                          (((chuck)))
                          (if (a) ((wood chuck)))
                          could chuck wood))
|#

(define member*
  (lambda (a l)
    (cond ((null? l) #f)
          ((atom? (car l))
           (or (eqan? a (car l))
               (member* a (cdr l))))
          (else
           (or (member* a (car l))
               (member* a (cdr l)))))))

(define leftmost
  (lambda (l)
    (cond ((null? l) '())
          ((atom? (car l))
           (car l))
          (else (leftmost (car l))))))
#|

(define eqlist?
  (lambda (l1 l2)
    (cond ((and (null? l1) (null? l2))
           #t)
          ((or (null? l1) (null? l2))
           #f)
          ((and (atom? (car l1)) (atom? (car l2)))
           (and (eqan? (car l1) (car l2))
                (eqlist? (cdr l1) (cdr l2))))
          ((or (atom? (car l1)) (atom? (car l2)))
           #f)
          (else
           (and (eqlist? (car l1) (car l2))
                (eqlist? (cdr l1) (cdr l2)))))))
|#

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

;;(eqlist? '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (soda))))

(define rember
  (lambda (s l)
    (cond ((null? l) '())
          ((equal? s (car l))
           (cdr l))
          (else (cons (car l)
                      (rember s (cdr l)))))))

;;;;;;; Chapter 6 - Shadows ;;;;;;;

(define numbered?
  (lambda (aexp)
    (cond ((atom? aexp) (number? aexp))
          (else
           (and (numbered? (car aexp))
                (numbered? (car (cdr (cdr aexp)))))))))

;;(numbered? '((2 - (3 * 5)) + (3 * 2)))

#|
;;;;; old value function for arithmetic expresions like (3 + 3)
(define value
  (lambda (nexp)
    (cond ((atom? nexp) nexp)
          ((eq? (operator nexp) '+)
           (+ (value (1st-sub-exp nexp))
              (value (2nd-sub-exp nexp))))
          ((eq? (operator nexp) '*)
           (* (value (1st-sub-exp nexp))
              (value (2nd-sub-exp nexp))))
          (else
           (expo (value (1st-sub-exp nexp))
                 (value (2nd-sub-exp nexp)))))))
|#

(define operator
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define 1st-sub-exp
  (lambda (aexp)
    (car aexp)))


(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (s)
    (cons '() s)))

(define zub1
  (lambda (s)
    (cdr s)))

(define blus
  (lambda (n1 n2)
    (cond ((sero? n2) n1)
          (else
           (blus (edd1 n1) (zub1 n2))))))

;;;;;;; Chapter 7 - Friends and Relations ;;;;;;;
#|

(define noteqany?
  (lambda (a lat)
    (cond ((null? lat) #t)
          ((eqan? a (car lat)) #f)
          (else
           (noteqany? a (cdr lat))))))

(define set?
  (lambda (lat)
    (cond ((null? lat) #t)
          (else
           (and (noteqany? (car lat) (cdr lat))
                (set? (cdr lat)))))))

(define member?
  (lambda (a lat)
    (cond ((null? lat) #f)
          (else
           (or (equal? a (car lat))
               (member? a (cdr lat)))))))

|#

(define set?
  (lambda (lat)
    (cond ((null? lat) #t)
          ((member? (car lat) (cdr lat)) #f)
          (else
           (set? (cdr lat))))))
;;;;;不要加入太多的逻辑层级，比如把 cond 的第二行并入 else 就多了一层 not: (and (not (member? ...)) ...);;;;;
;(set? '(apple 3 peaches 4 9 pears plum))

#|
我的版本...
(define makeset
  (lambda (lat)
    (cond ((null? lat) '())
          ((member? (car lat) (cdr lat))
           (cons (car lat)
                 (makeset (multirember (car lat) (cdr lat)))))
          (else
           (cons (car lat)
                 (makeset (cdr lat)))))))

(makeset '(apple peach pear peach plum apple lemon peach))
|#

(define makeset
  (lambda (lat)
    (cond ((null? lat) '())
          (else
           (cons (car lat)
                 (makeset
                  (multirember (car lat) (cdr lat))))))))

(define subset?
  (lambda (set1 set2)
    (cond ((null? set1) #t)
          (else
           (and (member? (car set1) set2)
                (subset? (cdr set1) set2))))))

;(subset? '(4 pounds of horseradish) '(four pounds chicken and 5 ounces horseradish))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))

;;(eqset? '(6 large chickens with wings) '(6 chickens with large wings))

(define intersect?
  (lambda (set1 set2)
    (cond ((null? set1) #f)
          (else
           (or (member? (car set1) set2)
               (intersect? (cdr set1) set2))))))

(define intersect
  (lambda (set1 set2)
    (cond ((null? set1) '())
          ((member? (car set1) set2)
           (cons (car set1)
                 (intersect (cdr set1) set2)))
          (else
           (intersect (cdr set1) set2)))))

;(intersect '(stewed tomatoes and macaroni) '(macaroni and cheese))
#|
我的版本
(define combine
  (lambda (set1 set2)
    (cond ((null? set1) set2)
          (else
           (cons (car set1)
                 (combine (cdr set1) set2))))))

(define union
  (lambda (set1 set2)
    (makeset (combine set1 set2))))
|#


(define union
  (lambda (set1 set2)
    (cond ((null? set1) set2)
          ((member? (car set1) set2)
           (union (cdr set1)
                  set2))
          (else
           (cons (car set1)
                 (union (cdr set1) set2))))))

;(union '(stewed tomatoes and macaroni casserole) '(macaroni and cheese))

(define xxx
  (lambda (set1 set2)
    (cond ((null? set1) '())
          ((member? (car set1) set2)
           (xxx (cdr set1)
                  set2))
          (else
           (cons (car set1)
                 (xxx (cdr set1) set2))))))

(define intersectall
  (lambda (l-set)
    (cond ((null? (cdr l-set)) (car l-set))
          (else
           (intersect (car l-set)
                      (intersectall (cdr l-set)))))))
#|
(intersectall '((a b c) (c a d e) (e f g h a b)))
(intersectall '((6 pears and)
                (3 peaches and 6 peppers)
                (8 pears and 6 plums)
                (and 6 prunes with some apples)))
|#

(define a-pair?
  (lambda (s)
    (cond ((atom? s) #f)
          ((null? s) #f)
          ((null? (cdr s)) #f)
          ((null? (cdr (cdr s))) #t)
          (else #f))))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define build
  (lambda (s1 s2)
    (cons s1
          (cons s2 '()))))

(define third
  (lambda (l)
    (car (cdr (cdr l)))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

;(fun? '((8 3) (4 2) (7 6) (6 2) (3 4)))
;(fun? '((4 3) (4 2) (7 6) (6 2) (3 4)))
;(fun? '((d 4) (b 0) (b 9) (e 5) (g 4)))

(define revpair
  (lambda (p)
    (build (second p)
           (first p))))

(define revrel
  (lambda (rel)
    (cond ((null? rel) '())
          (else
           (cons (revpair (car rel))
                 (revrel (cdr rel)))))))

;(revrel '((8 a) (pumpkin pie) (got sick)))

(define seconds
  (lambda (l)
    (cond ((null? l) '())
          (else
           (cons (car (cdr (car l)))
                 (seconds (cdr l)))))))

(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))


;;看到这个写法被震惊了
(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))))
#|
(fullfun? '((8 3) (4 4) (7 6) (6 2) (3 4)))
(fullfun? '((grape raisin) (plum prune) (stewed grape)))
(one-to-one? '((8 3) (4 4) (7 6) (6 2) (3 4)))
(one-to-one? '((grape raisin) (plum prune) (stewed grape)))
|#

;;;;;;; Chapter 8 - Lambda the Ultimate ;;;;;;;

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond ((null? l) '())
            ((test? a (car l)) (cdr l))
            (else
             (cons (car l)
                   ((rember-f test?) a (cdr l))))))))

;((rember-f eq?) 'tuna '(shrimp salad and tuna salad))

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond ((null? l) '())
            ((test? (car l) old)
             (cons new l))
            (else
             (cons (car l)
                   ((insertL-f test?) new old (cdr l))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond ((null? l) '())
            ((test? (car l) old)
             (cons old
                   (cons new
                         (cdr l))))
            (else
             (cons (car l)
                   ((insertR-f test?) new old (cdr l))))))))

#|
我写的版本一
(define insert-g
  (lambda (test?)
    (lambda (new old l)
      (lambda (side)
        ((side test?) new old l)))))

(((insert-g eq?) 'xxx 'tuna '(shrimp salad and tuna salad)) insertL-f)
(((insert-g eq?) 'xxx 'tuna '(shrimp salad and tuna salad)) insertR-f)
|#


(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

#|

(define rember
  (lambda (a lat)
    (cond ((null? lat) '())
          ((eqan? a (car lat))
           (cdr lat))
          (else (cons (car lat)
                      (rember a (cdr lat)))))))

|#

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond ((null? l) '())
            ((eq? old (car l))
             (seq new old (cdr l)))
            (else
             (cons (car l)
                   ((insert-g seq) new old (cdr l))))))))

(define seqS
  (lambda (new old l)
    (cons new l)))

(define yyy
  (lambda (a lat)
    ((insert-g seqrem) #f a lat)))

(define seqrem
  (lambda (old new l)
    l))
;;;;;; what you have just seen (see the codes above) is the power of abstraction ;;;;;;

(define atom-to-function
  (lambda (x)
    (cond ((eq? x '+) +)
          ((eq? x '*) *)
          (else expo))))

#|
;;;;;;comment out for creating the interpreter in Chapter 10;;;;;;
(define value
  (lambda (nexp)
    (cond ((atom? nexp) nexp)
          (else
           ((atom-to-function (operator nexp))
            (value (1st-sub-exp nexp))
            (value (2nd-sub-exp nexp)))))))
|#

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond ((null? lat) '())
            ((test? a (car lat))
             ((multirember-f test?) a (cdr lat)))
            (else
             (cons (car lat)
                   ((multirember-f test?) a (cdr lat))))))))

(define eq?-tuna
  (eq?-c 'tuna))

(define multiremberT
  (lambda (fun lat)
    (cond ((null? lat) '())
          ((fun (car lat))
           (multiremberT fun (cdr lat)))
          (else
           (cons (car lat)
                 (multiremberT fun (cdr lat)))))))

;;(multiremberT eq?-tuna '(shrimp salad tuna salad and tuna))

(define multirember&co
  (lambda (a lat col)
    (cond ((null? lat)
           (col '() '()))
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

(define a-friend
  (lambda (x y)
    (null? y)))

(define new-friend
  (lambda (newlat seen)
    (a-friend newlat
              (cons 'tuna seen))))

(define latest-friend
  (lambda (newlat seen)
    (a-friend (cons 'and newlat)
              seen)))

(define last-friend
  (lambda (x y)
    (length x)))

;(multirember&co 'tuna '(strawberries tuna and swordfish) a-friend)


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


(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond ((null? lat) '())
          ((eq? oldL (car lat))
           (cons new
                 (cons oldL
                       (multiinsertLR new oldL oldR (cdr lat)))))
          ((eq? oldR (car lat))
           (cons oldR
                 (cons new
                       (multiinsertLR new oldL oldR (cdr lat)))))
          (else
           (cons (car lat)
                 (multiinsertLR new oldL oldR (cdr lat)))))))

#|
我想当然的版本....(运行没问题，但没输出要求的结果)
(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond ((null? lat) (col '() '()))
          ((eq? oldL (car lat))
           (multiinsertLR&co new oldL oldR (cdr lat)
                          (lambda (inL inR)
                            (col (cons new
                                       (cons (car lat)
                                             inL))
                                 (cons (car lat)
                                       inR)))))
          ((eq? oldR (car lat))
           (multiinsertLR&co new oldL oldR (cdr lat)
                          (lambda (inL inR)
                            (col (cons (car lat)
                                       inL)
                                 (cons (car lat)
                                       (cons new
                                             inR))))))
          (else
           (multiinsertLR&co new oldL oldR (cdr lat)
                          (lambda (inL inR)
                            (col (cons (car lat)
                                       inL)
                                 (cons (car lat)
                                       inR))))))))
;(multiinsertLR&co 'xxx 'and 'tuna '(tuna strawberries and swordfish) (lambda (l r) l))
|#


(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond ((null? lat) (col '() 0 0))
          ((eq? oldL (car lat))
           (multiinsertLR&co new oldL oldR (cdr lat)
                          (lambda (newlat numL numR)
                            (col (cons new
                                       (cons oldL newlat))
                                 (add1 numL)
                                 numR))))
          ((eq? oldR (car lat))
           (multiinsertLR&co new oldL oldR (cdr lat)
                          (lambda (newlat numL numR)
                            (col (cons oldR
                                       (cons new newlat))
                                 numL
                                 (add1 numR)))))
          (else
           (multiinsertLR&co new oldL oldR (cdr lat)
                          (lambda (newlat numL numR)
                            (col (cons (car lat) newlat)
                                 numL
                                 numR)))))))

;(multiinsertLR&co 'salty 'fish 'chips '(chips and fish or fish and chips) (lambda (lat l r) lat))
#|
(define even?
  (lambda (n)
    (cond ((zero? n) #t)
          ((zero? (sub1 n)) #f)
          (else
           (even? (- n 2))))))
|#

(define new-quotient
  (lambda (n1 n2)
    (cond ((lthan n1 n2) 0)
          (else
           (add1 (new-quotient (- n1 n2) n2))))))

(define even?
  (lambda (n)
    (= (* (new-quotient n 2) 2) n)))

(define evens-only*
  (lambda (l)
    (cond ((null? l) '())
          ((atom? (car l))
           (cond ((even? (car l))
                  (cons (car l)
                        (evens-only* (cdr l))))
                 (else
                  (evens-only* (cdr l)))))
          (else
           (cons (evens-only* (car l))
                 (evens-only* (cdr l)))))))

;(evens-only* '((3 5 6 (9 (8) 7 4 3)) 43 5 2 (62 3 67)))
;(evens-only* '((9 1 2 8) 3 10 ((9 9 ) 7 6) 2))

#|
;;;我的版本;;;
(define evens-only*&co
  (lambda (l col)
    (cond ((null? l) (col '() 0 1))
          ((atom? (car l))
           (cond ((even? (car l))
                  (evens-only*&co (cdr l)
                                  (lambda (new-l s pdct)
                                    (col (cons (car l)
                                               new-l)
                                         s
                                         (* (car l) pdct)))))
                 (else
                  (evens-only*&co (cdr l)
                                  (lambda (new-l s pdct)
                                    (col new-l
                                         (+ (car l) s)
                                         pdct))))))
          (else
           (evens-only*&co (cdr l)
                           (evens-only*&co (car l)
                                           (lambda (new-l1 s1 pdct1)
                                             (lambda (new-l2 s2 pdct2)
                                               (col (cons new-l1 new-l2)
                                                    (+ s1 s2)
                                                    (* pdct1 pdct2))))))))))
|#

(define evens-only*&co
  (lambda (l col)
    (cond ((null? l) (col '() 0 1))
          ((atom? (car l))
           (cond ((even? (car l))
                  (evens-only*&co (cdr l)
                                  (lambda (new-l s pdct)
                                    (col (cons (car l)
                                               new-l)
                                         s
                                         (* (car l) pdct)))))
                 (else
                  (evens-only*&co (cdr l)
                                  (lambda (new-l s pdct)
                                    (col new-l
                                         (+ (car l) s)
                                         pdct))))))
          (else
           (evens-only*&co (car l)
                           (lambda (new-l1 s1 pdct1)
                             (evens-only*&co (cdr l)
                                             (lambda (new-l2 s2 pdct2)
                                               (col (cons new-l1 new-l2)
                                                    (+ s1 s2)
                                                    (* pdct1 pdct2))))))))))

(define the-last-friend
  (lambda (newl s p)
    (cons s
          (cons p
                newl))))
;(evens-only*&co '((9 1 2 8) 3 10 ((9 9 ) 7 6) 2) the-last-friend)
;(evens-only*&co '((3 5 6 (9 (8) 7 4 3)) 43 5 2 (62 3 67)) the-last-friend)

;;;;;;; Chapter 9 - ... and Again, and Again, and Again, ... ;;;;;;;

#|
我的盲写版本
(define find-nth
  (lambda (n lat)
    (cond ((zero? (sub1 n)) (car lat))
          (else
           (find-nth (sub1 n) (cdr lat))))))

(define looking-until-notnum
  (lambda (a lat)
    (cond ((number? a)
           (looking-until-notnum
            (find-nth a lat)
            lat))
          (else
           a))))

(define looking
  (lambda (a lat)
    (cond ((null? lat) #f)
          (else
           (eq? a
                (looking-until-notnum (car lat)
                                      lat))))))
|#
(define pick-up
  (lambda (n lat)
    (cond ((zero? (sub1 n)) (car lat))
          (else
           (pick-up (sub1 n) (cdr lat))))))

(define keep-looking
  (lambda (a ax lat)
    (cond ((number? ax)
           (keep-looking a (pick-up ax lat) lat))
          (else
           (eq? a ax)))))

(define looking
  (lambda (a lat)
    (keep-looking a (pick-up 1 lat) lat)))

;(looking 'caviar '(6 2 grits caviar 5 7 3))

#|
我的盲写版本
(define shift
  (lambda (l)
    (cond ((null? l) '())
          ((atom? (car l)) l)
          (else
           (cons (leftmost l)
                 (cons (cons (car (cdr (car l)))
                             (cons (car (cdr l))
                                   '()))
                       '()))))))
|#

(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))

(define align
  (lambda (pora)
    (cond ((atom? pora) pora)
          ((a-pair? (first pora))
           (align (shift pora)))
          (else (build (first pora)
                       (align (second pora)))))))

(define length*
  (lambda (pora)
    (cond ((atom? pora) 1)
          (else
           (+ (length* (first pora))
              (length* (second pora)))))))

(define weight*
  (lambda (pora)
    (cond ((atom? pora) 1)
          (else
           (+ (* (weight* (first pora)) 2)
              (weight* (second pora)))))))

(define shuffle
  (lambda (pora)
    (cond ((atom? pora) pora)
          ((a-pair? (first pora))
           (shuffle (revpair pora)))
          (else (build (first pora)
                       (shuffle (second pora)))))))

(define length
  (lambda (l)
    (cond ((null? l) 0)
          (else (add1 (length (cdr l)))))))

(define eternity
  (lambda (x)
    (eternity x)))

#|
;;;;; scratch 1 ;;;;;

(lambda (l)
  (cond ((null? l) 0)
        (else
         (add1 ((lambda (l)
                  (cond ((null? l) 0)
                        (else
                         (add1 (eternity l)))))
                (cdr l))))))


;;;;; scratch 2 ;;;;;

((lambda (length)
   (lambda (l)
     (cond ((null? l) 0)
           (else
            (add1 (length
                   (cdr l)))))))
 ((lambda (length)
    (lambda (l)
      (cond ((null? l) 0)
            (else
             (add1 (length
                    (cdr l)))))))
  eternity))

;;;;; scratch 3 ;;;;;

((lambda (mk-length)
   (mk-length
    (mk-length
     (mk-length eternity))))
 (lambda (length)
   (lambda (l)
     (cond ((null? l) 0)
           (else
            (add1 (length
                   (cdr l))))))))

;;;;; scratch 4 ;;;;;

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond ((null? l) 0)
           (else
            (add1 ((mk-length mk-length)
                   (cdr l))))))))

;;;;; scratch 5 ;;;;;

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (length)
      (lambda (l)
        (cond ((null? l) 0)
              (else
               (add1 (length
                      (cdr l)))))))
    (mk-length mk-length))))

;;;;; scratch 6 ;;;;;

((lambda (mk-length)
    (mk-length mk-length))
  (lambda (mk-length)
    ((lambda (length)
       (lambda (l)
         (cond ((null? l) 0)
               (else
                (add1 (length
                       (cdr l)))))))
     (lambda (x)
       ((mk-length mk-length) x)))))     ;;;;用 lambda 保护起来等一下再算 lazy evaluation?

;;;;; scratch 7 ;;;;;

((lambda (y)
   ((lambda (mk-length)
      (mk-length mk-length))
    (lambda (mk-length)
      (y
       (lambda (x)
         ((mk-length mk-length) x))))))
 
 (lambda (length)
   (lambda (l)
     (cond ((null? l) 0)
           (else
            (add1 (length
                   (cdr l))))))))

|#

;;;;;;; Chapter 10 - What Is the Value of All of This ? ;;;;;;;

(define new-entry build)

#|
我的版本
(define lookup-in-entry
  (lambda (name entry)
    (cond ((null? (first entry)) '())
          ((eq? name (car (first entry)))
           (car (second entry)))
          (else
           (lookup-in-entry name
                            (new-entry (cdr (first entry))
                                       (cdr (second entry))))))))
|#

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond ((null? names) (entry-f name))
          ((eq? name (car names))
           (car values))
          (else
           (lookup-in-entry-help name
                                 (cdr names)
                                 (cdr values)
                                 entry-f)))))

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
                          (first entry)
                          (second entry)
                          entry-f)))


(define extend-table cons)

#|
我的版本
(define lookup-in-table
  (lambda (name entry table-f)
    (cond ((null? table) (table-f name))
          ((member? name (first (car table)))
           (lookup-in-entry name (car table) table-f))
          (else
           (looup-in-table name
                           (cdr table)
                           table-f)))))
|#


(define lookup-in-table
  (lambda (name table table-f)
    (cond ((null? table) (table-f name))
          (else
           (lookup-in-entry name
                            (car table)
                            (lambda (n)
                              (lookup-in-table n
                                               (cdr table)
                                               table-f)))))))

(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else
       (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    (cond ((number? e) *const)
          ((eq? e '#t) *const)
          ((eq? e '#f) *const)
          ((eq? e 'car) *const)
          ((eq? e 'cdr) *const)
          ((eq? e 'eq?) *const)
          ((eq? e 'null?) *const)
          ((eq? e 'atom?) *const)
          ((eq? e 'add1) *const)
          ((eq? e 'sub1) *const)
          ((eq? e 'zero?) *const)
          ((eq? e 'cons) *const)
          ((eq? e 'number?) *const)
          (else
           *identifier))))

(define list-to-action
  (lambda (e)
    (cond ((atom? (car e))
           (cond ((eq? (car e) 'cond) *cond)
                 ((eq? (car e) 'lambda) *lambda)
                 ((eq? (car e) 'quote) *quote)
                 (else
                  *application)))
          (else
           *application))))


(define *const
  (lambda (e table)
    (cond ((number? e) e)
          ((eq? e '#t) #t)
          ((eq? e '#f) #f)
          (else
           (build 'primitive e)))))

(define text-of second)

(define *quote
  (lambda (e table)
    (text-of e)))

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define initial-table
  (lambda (name)
    (car '())))

(define *lambda
  (lambda (e table)
    (build 'non-primitive
           (cons table (cdr e)))))

(define table-of first)

(define formals-of second)

(define body-of third)

(define else?
  (lambda (e)
    (cond ((atom? e) (eq? e 'else))
          (else #f))))

(define question-of first)

(define answer-of second)

(define evcon
  (lambda (lines table)
    (cond ((else? (question-of (car lines)))
           (meaning (answer-of (car lines))
                    table))
          ((meaning (question-of (car lines))
                    table)
           (meaning (answer-of (car lines))
                    table))
          (else
           (evcon (cdr lines) table)))))

(define cond-lines-of cdr)

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))


(define evlis
  (lambda (args table)
    (cond ((null? args) '())
          (else
           (cons (meaning (car args) table)
                 (evlis (cdr args) table))))))

(define function-of car)

(define arguments-of cdr)

(define primitive?
  (lambda (l)
    (eq? (first l) 'primitive)))

(define non-primitive?
  (lambda (l)
    (eq? (first l) 'non-primitive)))

(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table (new-entry (formals-of closure)
                                      vals)
                           (table-of closure)))))

(define new-atom?
  (lambda (x)
    (cond ((atom? x) #t)
          ((null? x) #f)
          ((eq? (car x) 'primitive) #t)
          ((eq? (car x) 'non-primitive) #t)
          (else #f))))

(define apply-primitive
  (lambda (f vals)
    (cond ((eq? f 'cons)
           (cons (first vals) (second vals)))
          ((eq? f 'car)
           (car (first vals)))
          ((eq? f 'cdr)
           (cdr (first vals)))
          ((eq? f 'null?)
           (null? (first vals)))
          ((eq? f 'eq?)
           (eq? (first vals) (second vals)))
          ((eq? f 'atom?)
           (new-atom? (first vals)))
          ((eq? f 'zero?)
           (zero? (first vals)))
          ((eq? f 'add1)
           (add1 (first vals)))
          ((eq? f 'sub1)
           (sub1 (first vals)))
          ((eq? f 'number?)
           (number? (first vals))))))


(define apply
  (lambda (func vals)
    (cond ((primitive? func)
           (apply-primitive
            (second func) vals))
          ((non-primitive? func)
           (apply-closure
            (second func) vals)))))


(define *application
  (lambda (e table)
    (apply (meaning (function-of e) table)
           (evlis (arguments-of e) table))))


(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))


(define value
  (lambda (e)
    (meaning e '())))


(value '(cons 6 (quote (a b c))))

;;;;;;;;;;;;;; End of TLS ;;;;;;;;;;;;;;





