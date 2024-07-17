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

;; Panel 17


