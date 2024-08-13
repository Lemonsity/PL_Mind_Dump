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

(define rember
  (λ (x l)
    (cond
      [(null? l) l] ;; or [(null? l) null]
      [(eq-car? l x) (cdr l)]
      [else (cons (car l)
                  (rember x (cdr l)))])))

(define rembero
  (λ (x l out)
    (conde
     [(nullo l) (nullo out)]
     [(eq-caro l x) (cdro l out)]
     [(fresh (a d d-out)
             (conso a d l) ; destructing a pair
             (rembero x d d-out)
             (conso a d-out out))]))) ; constructing a pair

(define surpriseo
  (λ (s)
    (rembero s '(a b c) '(a b c))))
