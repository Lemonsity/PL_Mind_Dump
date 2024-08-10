#lang racket

(require "Chapter-03-Def.rkt")

(provide (all-defined-out))
(provide (all-from-out "Chapter-03-Def.rkt"))

(define mem
  (λ (x l)
    (cond
      [(null? l) #f]
      [(eq-car? l x) l]
      [else (mem x (cdr l))])))

(define memo
  (λ (x l out)
    (conde
     [(nullo l) fail]
     [(eq-caro l x) (== l out)]
     [(fresh (d)
             (cdro l d)
             (memo x d out))])))
