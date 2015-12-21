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

#|

(define rember
  (lambda (a lat)
    (cond ((null? lat) '())
          ((eqan? a (car lat))
           (cdr lat))
          (else (cons (car lat)
                      (rember a (cdr lat)))))))

|#

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















