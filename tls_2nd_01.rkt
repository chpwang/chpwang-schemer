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


(define multiinsertR
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((equal? old (car lat))
           (cons old
                 (cons new
                       (multiinsertR new old (cdr lat)))))
          (else (cons (car lat)
                      (multiinsertR new old (cdr lat)))))))


(define multiinsertL
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((equal? old (car lat))
           (cons new
                 (cons old
                       (multiinsertL new old (cdr lat)))))
          (else (cons (car lat)
                      (multiinsertL new old (cdr lat)))))))

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

(define length
  (lambda (lat)
    (cond ((null? lat) 0)
          (else (add1 (length (cdr lat)))))))

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

(define value
  (lambda (nexp)
    (cond ((atom? nexp) nexp)
          (else
           ((atom-to-function (operator nexp))
            (value (1st-sub-exp nexp))
            (value (2nd-sub-exp nexp)))))))

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













