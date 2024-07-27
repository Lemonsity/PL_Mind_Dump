#lang racket

(require "Chapter-01-Def.rkt")

(provide (all-defined-out))
(provide (all-from-out "Chapter-01-Def.rkt"))

(define caro
  (λ (p a)
    (fresh (d)
           (== (cons a d) p))))

(define cdro
  (λ (p d)
    (fresh (a)
           (== (cons a d) p))))

(define car& (λ (p k) (k (car p))))
(define cdr& (λ (p k) (k (cdr p))))

(define conso
  (λ (a d p)
    (== (cons a d) p)))

(define nullo
  (λ (l)
    (== '() l)))

(define eqo
  (λ (x y)
    (== x y)))

(define pairo
  (λ (p)
    (fresh (x y)
           (conso x y p))))

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

