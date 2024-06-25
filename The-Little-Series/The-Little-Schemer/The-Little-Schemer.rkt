#lang racket

#|
The Little Schemer Template

Here we implement some functions from The Little Schemer by D Friedman and M Felleisen

Note The Little Schemer's target languages are LISP and Scheme
We use Typed Racker here. Some functions may not behave as expected
|#

;(: atom? (-> Any Boolean))
(define atom?
  (λ (x)
    (and (not (pair? x)) (not (null? x)))))

;(: lat? (-> (Listof Any) Boolean))
(define lat?
  (λ (list)
    (cond
      [(null? list) #t]
      [(atom? (car list)) (lat? (cdr list))]
      [else #f]
    )
  )
)

;(: remove-member (-> Symbol (Listof Symbol) (Listof Symbol)))
(define remove-member
  (λ (a l)
    (cond
      [(null? l) '()]
      [(equal? a (car l)) (cdr l)]
      [else (cons (car l) (remove-member a (cdr l)))]
    )
  )
)

;(: firsts (-> (Listof (Listof Symbol)) (Listof Symbol)))
(define firsts
  (λ (lat)
    (cond
      [(null? lat) '()]
      [else (cons (car (car lat)) (firsts (cdr lat)))]
    )
  )
)

;(: insertR (-> Symbol Symbol (Listof Symbol) (Listof Symbol)))
(define insertR
  (λ (new old lat)
    (cond
      [(null? lat) (quote ())]
      [(equal? old (car lat)) (cons (car lat) (cons new (cdr lat)))]
      [else (cons (car lat) (insertR new old (cdr lat)))]
    )
  )
)

;(: o+ (-> Integer Integer Integer))
(define o+
  (λ (n m)
    (cond
      [(zero? n) m]
      [else
        (add1 (o+ (sub1 n) m))
      ]
    )
  )
)

;(: o- (-> Integer Integer Integer))
(define o-
  (λ (n m)
    (cond
      [(zero? m) n]
      [else (o- (sub1 m) (sub1 n))]
    )
  )
)

;(: o* (-> Integer Integer Integer))
(define o*
  (λ (n m)
    (cond
      [(zero? m) 1]
      [else (o+ n (o* n (sub1 m)))]
    )
  )
)

;(: addtup (-> (Listof Integer) Integer))
(define addtup
  (λ (tup)
    (cond
      [(null? tup) 0]
      [else (o+ (car tup) (addtup (cdr tup)))]
    )
  )
)

;(: rempick (-> Integer (Listof Symbol) (Listof Symbol)))
(define rempick
  (λ (n lat)
    (cond
      [(zero? (sub1 n)) (cdr lat)]
      [else
       (cons
        (car lat)
        (rempick (sub1 n) (cdr lat)))]
    )
  )
)

;(: tup+ (-> (Listof Integer) (Listof Integer) (Listof Integer)))
(define tup+
  (λ (tup1 tup2)
    (cond
      [(null? tup1) tup2]
      [(null? tup2) tup1]
      [else (cons (o+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))]
    )
  )
)

;(: o< (-> Integer Integer Boolean))
(define o<
  (λ (a b)
    (cond
      [(zero? b) #f]
      [(zero? a) #t]
      [else (o< (sub1 a) (sub1 b))]
    )
  )
)

;(: o> (-> Integer Integer Boolean))
(define o>
  (λ (a b)
    (cond
      [(zero? a) #f]
      [(zero? b) #t]
      [else (o> (sub1 a) (sub1 b))]
    )
  )
)

;(: o= (-> Integer Integer Boolean))
(define o=
  (λ (a b)
    (cond
      [(o< a b) #f]
      [(o> a b) #f]
      [else #t]
    )
  )
)

; Technically this function only takes positive intergers as input
; We using else defined o- only because we are following the textbook
;(: o/ (-> Integer Integer Integer))
(define o/
  (λ (n m)
    (cond
      [(o< n m) 0]
      [else (add1 (o/ (o- n m) m))]
    )
  )
)

;(: o^ (-> Integer Integer Integer))
(define o^
  (λ (a b)
    (cond
      [(zero? b) 1]
      [else (o* a (o^ a (sub1 b)))]
    )
  )
)

;(: my-length (All (a) (-> (Listof a) Integer)))
(define my-length
  (λ (l)
    (cond
      [(null? l) 0]
      [else (add1 (my-length (cdr l)))]
    )
  )
)

#|
An interesting thought. What would happen when we start to incorporate context into typing judgement?
In some langauges, 
|#
;(: pick (-> Integer (Listof Symbol) Symbol))
(define pick
  (λ (n lat)
    (cond
      [(zero? (sub1 n)) (car lat)]
      [else (pick (sub1 n) (cdr lat))]
    )
  )
)

;(: no-nums (-> (Listof Any) (Listof Any)))
(define no-nums
  (λ (lat)
    (cond
      [(null? lat) '()]
      [(number? (car lat)) (cdr lat)]
      [else (cons (car lat) (no-nums (cdr lat)))]
    )
  )
)

;(: all-nums (-> (Listof Any) (Listof Any)))
(define all-nums
  (λ (lat)
    (cond
      [(null? lat) '()]
      [(number? (car lat)) (cons (car lat) (all-nums (cdr lat)))]
      [else (all-nums (cdr lat))]
    )
  )
)

;(: eqan? (-> (U Integer Symbol) (U Integer Symbol) Boolean))
(define eqan?
  (λ (a1 a2)
    (cond
      [(and (number? a1) (number? a2)) (o= a1 a2)]
      [(or (number? a1) (number? a2)) #f]
      [else (eq? a1 a2)]
    )
  )
)

;(: occur (-> Symbol (Listof Symbol) Integer))
(define occur
  (λ (a lat)
    (cond
      [(null? lat) 0]
      [(eq? a (car lat)) (add1 (occur a (cdr lat)))]
      [else (occur a (cdr lat))]
    )
  )
)

;(: rember* Any)
(define rember*
  (λ (a l)
    (cond
      [(empty? l) '()]
      [(list? (car l)) (cons (rember* a (car l)) (rember* a (cdr l)))]
      [(eq? (car l) a) (rember* a (cdr l))]
      [else (cons (car l) (rember* a (cdr l)))]
    )
  )
)

(define insertR*
  (λ (new old l)
    (cond
      [(empty? l) '()]
      [(list? l) (cons (insertR* new old (car l)) (insertR* new old (cdr l)))]
      [(eq? old (car l)) (cons (car l) (cons new (insertR* new old (cdr l))))]
      [else (cons (car l) (insertR* new old (cdr l)))]
    )
  )
)

(define occur*
  (λ (a l)
    (cond
      [(empty? l) 0]
      [(atom? (car l))
       (cond
         [(eq? a (car l)) (add1 (occur* a (cdr l)))]
         [else (occur* a (cdr l))]
       )
      ]
      [else (o+ (occur* a (car l)) (occur* a (cdr l)))]
    )
  )
)

(define subst*
  (λ (new old l)
    (cond
      [(empty? l) '()]
      [(atom? (car l))
       (cond
         [(eq? old (car l)) (cons new (subst* new old (cdr l)))]
         [else (cons (car l) (subst* new old (cdr l)))]
       )
      ]
      [else (cons (subst* new old (car l)) (subst* new old (cdr l)))]
    )
  )
)

(define member*
  (λ (a l)
    (cond
      [(null? l) #f]
      [(atom? (car l))
       (cond
         [(eq? a (car l)) #t]
         [else (member* a (cdr l))]
       )
      ]
      [else (or (member* a (car l)) (member* a (cdr l)))]
    )
  )
)

(define leftmost
  (λ (l)
    (cond
      [(atom? (car l)) (car l)]
      [(list? (car l)) (leftmost (car l))]
    )
  )
)

#|
The little schemer reduces this function significantly by making mutual recursive call
I choose to not do it here, because I think it hides the steps it taks to get there

One can simplify with mutual recursive call to equal? that accepts any kind of input
|#
(define eqlist?
  (λ (l1 l2)
    (cond
      [(and (null? l1) (null? l2)) #t]
      [(or (not (null? l1)) (not (null? l2))) #f]
      [(and (atom? (car l1)) (atom? (car l2)))
       (and (eq? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))]
      [(or (not (atom? (car l1)) (atom? (car l2)))) #f]
      [(and (list? (car l1)) (list? (car l2)))
       (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))]
      [else #f]
    )
  )
)


; ================================= 6. Shadow ================================= 
(define numbered?
  (λ (aexp)
    (cond
      [(atom? aexp) (number? aexp)]
      [else (and
             (numbered? (car aexp))
             (numbered? (caddr aexp)) ; (caddr x) = (car (cdr (cdr x)))
             ; One can potentially check if the operator is correct here
            )
      ]
    )
  )
)
      
