#lang racket

(require minikanren)
(require "Chapter-02-Def.rkt")

(provide (all-defined-out))
(provide (all-from-out "Chapter-02-Def.rkt"))

(define listo
  (λ (l)
    (conde
     [(nullo l) succeed]
     [(pairo l) (fresh (d)
                       (cdro l d)
                       (listo d))]
     [fail])))

(define lol?
  (λ (l)
    (cond
      [(null? l) #t]
      [(list? (car l)) (lol? (cdr l))]
      [else #f])))

(define lolo
  (λ (l)
    (conde
     [(nullo l) succeed]
     [(fresh (a)
             (caro l a)
             (listo a))
      (fresh (d)
             (cdro l d)
             (lolo d))]
     [fail])))

(define twino
  (λ (p)
    (fresh (a d)
           (conso a d p)
           ;; vv Make sure tail is one-element long
           (conso a '() d))))

(define twino-
  (λ (p)
    (fresh (x)
           (== `(,x ,x) p))))

(define loto
  (λ (l)
    (conde
     [(nullo l) succeed]
     [(fresh (a)
             (caro l a)
             (twino a))
      (fresh (d)
             (cdro l d)
             (loto d))]
     [fail])))

(define listofo
  (λ (predo l)
    (conde
     [(nullo l) succeed]
     [(fresh (a)
             (caro l a)
             (predo a))
      (fresh (d)
             (cdro l d)
             (listofo predo d))]
     [fail])))

(define loto-
  (λ (l)
    (listofo twino l)))

(define eq-car?
  (λ (l x)
    (eq? (car l) x)))

(define member?
  (λ (x l)
    (cond
      [(null? l) #f]
      [(eq-car? l x) #t]
      [else (member? x (cdr l))])))


(define eq-caro
  (λ (l x)
    (caro l x)))

(define membero
  (λ (x l)
    (conde
     [(nullo l) fail]
     [(eq-caro l x) succeed]
     [(fresh (d)
             (cdro l d)
             (membero x d))])))

(define identity-
  (λ (l)
    (run* (item)
          (membero item l))))

(define pmembero-bad
  (λ (x l)
    (conde
     [(nullo l) fail]
     [(eq-caro l x) (fresh (d)
                           (cdro l d)
                           (listo d))]
     [(fresh (d)
             (cdro l d)
             (pmembero-bad x d))])))

(define pmembero
  (λ (x l)
    (conde
     [(eq-caro l x) (fresh (a d)
                           (cdro l `(,a . ,d)))]
     [(eq-caro l x) (cdro l '())]
     [(fresh (d)
             (cdro l d)
             (pmembero x d))])))

(define length-0 '())
(define length-1 '(()))
(define length-2 '(() ()))
(define length-3 '(() () ()))

(define my-lengtho
  (λ (l len) 
    (conde
     [(nullo l) (== len '())]
     [(fresh (d sublen)
             (cdro l d)
             (conso '() sublen len)
             (my-lengtho d sublen))])))


(define my-pmembero-helper
  (λ (x l l-len-sub1)
    (conde
     [(nullo l) fail]
     [(eq-caro l x) (fresh (d)                           
                           (cdro l d)
                           (my-lengtho d l-len-sub1))]
     [(fresh (d d-len-sub1)
             (cdro l d)
             (conso '() d-len-sub1 l-len-sub1)
             (my-pmembero-helper x d d-len-sub1))])))

(define my-pmembero-start-with
  (λ (x l start-len)
    (conde
     [(my-pmembero-helper x l start-len) succeed]
     [(my-pmembero-start-with x l (cons '() start-len))])))

(define my-pmembero
  (λ (x l)
    (my-pmembero-start-with x l length-0)))

(define first-value
  (λ (l)
    (run 1 (y)
         (membero y l))))

;; TODO I have strong suspicion about this one
(define memberrevo
  (λ (x l)
    (conde
     [(nullo l) fail]
     [succeed (fresh (d)
                     (cdro l d)
                     (memberrevo x d))]
     [(eq-caro l x)])))

(define reverse-list
  (λ (l)
    (run* (y)
          (memberrevo y l))))
