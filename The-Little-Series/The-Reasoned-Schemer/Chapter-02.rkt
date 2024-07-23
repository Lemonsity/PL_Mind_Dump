#lang racket

(require "Racket-miniKanren/miniKanren/mk.rkt")

;; Panel 1
;; Function application
(let ([x (λ (a) a)]
      [y 'c])
  (x y))

;; Panel 2, 3
;; Quasiquote & Unquote
(run* (q)
      (fresh (x y)
             ;; [`] to quasiquote
             ;; [,] to unquote
             (== `(,x ,y) q)))

(run* (r)
      (fresh (v w)
             (== (let ([x v]
                       [y w])
                   `(,x ,y))
                 r)))
;; ^^ is equivalent to vv after [let] substitution
(run* (r)
      (fresh (v w)
             (== `(,v ,w) r)))

;; Panel 3, 4 skipped

;; Panel 6, 7, 8, 9
#| [caro]
The main focus here is that we are not using built-in
form [car]
We are forcing pattern matching with [cons]
|#
(define caro
  (λ (p a)
    (fresh (d)
           (== (cons a d) p))))
(run* (r)
      (caro '(a c o r n) r))
(run* (q)
      (caro '(a c o r n) 'a)
      (== #t q))
(run* (q)
      (caro '(a c o r n) 'not-a) ;; This cause whole to fail
      (== #t q))
(run* (r)
      (fresh (x y)
             (caro `(,r ,y) x) ;; r associated with x
             (== 'pear x))) ;; x associated with 'pear
;; This generates only a single value
;; But [_.0] can take on infinite values
(run* (r)
       (caro r 'a))

;; Panel 10, 11, 12
;; Using [caro] like functional programming
(run* (r)
      (fresh (x y)
             (caro '(grape raisin pear) x)
             (caro '((a) (b) (c)) y)
             (== (cons x y) r)))
;; ^^ is basically equivalent to vv
(cons
 (car '(grape raisin pear))
 (car '((a) (b) (c))))

;; Panel 13, 14, 15, 16
(define cdro
  (λ (p d)
    (fresh (a)
           (== (cons a d) p))))

(car (cdr '(a c o r n)))
;; Unnesting
;;   Durning [ (car (cdr ...)) ]
;;   into [ cdro ... ] then [ caro ... ]
;;   is called Unnesting
(run* (r)
      (fresh (r1 r2)
             (cdro '(a c o r n) r1)
             (caro r1 r2)
             (== r2 r)))
;; Unnesting is similar to CPS
;; Notice where [r1] appears in both cases
(define car& (λ (p k) (k (car p))))
(define cdr& (λ (p k) (k (cdr p))))
(cdr& '(a c o r n)
     (λ (r1) (car& r1
                   identity)))

;; Panel 17, 18
(run* (r)
      (fresh (x y)
             (cdro '(grape raisin pear) x)
             (caro '((a) (b) (c)) y)
             (== (cons x y) r)))

;; Panel 19
(run* (q)
      ;; cdro line is always succeed
      (cdro '(a c o r n) '(c o r n))
      (== #t q))

;; Panel 20
;; [x] is substituted with 'o
(run* (x)
      (cdro '(c o r n) `(,x r n)))
;; ^^ becomes vv when substitute the value of [(cdro ...)]
(run* (x)
      (fresh (a)
             (== (cons a `(,x r n)) '(c o r n))))

;; Panel 21
;; Specifiying all arguments will force a value out
(run* (l)
      (fresh (x)
             (cdro l '(c o r n))
             (caro l x)
             (== 'a x)))

;; Panel 22 - 28
(define conso
  (λ (a d p)
    (== (cons a d) p)))
;; Using [conso] like function
(run* (l)
      (conso '(a b c) '(d e) l))
;; Using [conso] to query for [car]
(run* (var-a)
      (conso var-a '(b c) '(a b c)))
;; Using [conso] to search for value
(run* (l)
      (fresh (x y z)
             (== `(e a d ,x) l) ;; 1. [l] is now a 4-element list
             (conso y `(a ,z c) l))) ;; 2. The last element of [l] is forced to be ['c]
(run* (x)
      (conso x `(a ,x c) `(d a ,x c)))
(run* (l)
      (fresh (x)
             (conso x `(a ,x c) l)
             (== `(d a ,x c) l)))

;; Panel 29
;; b e a n s
(run* (l)
      (fresh (d x y w s)
             (conso w '(a n s) s)
             (cdro l s)
             (caro l x)
             (== 'b x)
             (cdro l d)
             (caro d y)
             (== 'e y)))

;; Panel 30 - 35
;; [nullo]
;; [nullo] no longer takes an additional argument for the "result"
;; Whether the goal [(nullo ...)] succeed or fail will indicate the "result"
(define nullo
  (λ (l)
    (== '() l)))
(run* (q)
      (nullo '()) ;; This simply succeed and skipped
      (== #t q)) ;; associate [q] to [#t]
(run* (l)
      (nullo l)) ;; associate [l] with ['()]

;; Panel 36 - 40
(define eqo
  (λ (x y)
    (== x y)))
;; [eqo] check for equality
(run* (q)
      (eqo 'pear 'plum))
(run* (q)
      (eqo 'pear 'pear)
      (== #t q))

;; Panel 41 - 51 skipped
;; Mostly review of pair

;; Panel 52
;; fresh variable will be reify
(run* (r)
      (fresh (x y)
             (== (cons x (cons y 'salad)) r)))

;; Panel 53
;; [pairo]
;; Determines whether an expression is a pair
(define pairo
  (λ (p)
    (fresh (x y)
           (conso x y p))))

;; Panel 54, 55
;; Succeed because top-level constructor is [cons]
(run* (q)
      (pairo (cons q q))
      (== #t q))
;; Fails because ['()] is not a pair
(run* (q)
      (pairo '())
      (== #t q))
;; Fail because an atom is not a pair
(run* (q)
      (pairo 'pair)
      (== #t q))

;; Panel 57
;; Most General Unifier of [pairo]
(run* (p)
      (pairo p))
;; Associates [p] with [(_.0 . _.1)]

;; Panel 58
(run* (a)
      (pairo (cons a 'pear)))
;; Associate [a] with [_.0]

;; Panel 59
;; Try rewrite [caro, cdro, pairo] using [conso]
(define caro-
  (λ (p a)
    (fresh (d)
           (conso a d p))))
(define cdro-
  (λ (p d)
    (fresh (a)
           (conso a d p))))
(define pairo-
  (λ (p)
    (fresh (a d)
           (conso a d p))))

