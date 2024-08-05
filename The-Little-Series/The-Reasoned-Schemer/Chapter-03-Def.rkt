#lang racket

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

