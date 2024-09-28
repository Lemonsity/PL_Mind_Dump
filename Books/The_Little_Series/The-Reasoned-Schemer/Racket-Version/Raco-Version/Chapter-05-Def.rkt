#lang racket

(require minikanren)
(require "Chapter-04-Def.rkt")

(provide (all-defined-out))
(provide (all-from-out "Chapter-04-Def.rkt"))

(define append-
  (λ (l s)
    (cond
      [(null? l) s]
      [else (cons (car l)
                  (append (cdr l) s))])))

(define appendo1
  (λ (l s out)
    (conde
     [(nullo l) (== s out)]
     [(fresh (a d d-out)
             
             (caro l a) ; Might be able to replace these two lines with
             (cdro l d) ; [(conso a d l)]
             
             (appendo1 d s d-out)
             (conso a d-out out))])))

(define appendo2
  (λ (l s out)
    (conde
     [(nullo l) (== s out)]
     [(fresh (a d d-out)
             (conso a d l)
             (appendo2 d s d-out)
             (conso a d-out out))])))

(define appendo
  (λ (l s out)
    (conde
     [(nullo l) (== s out)]
     [(fresh (a d d-out)
             (conso a d l)
             (conso a d-out out)
             (appendo d s d-out))])))

(define swappendo
  (λ (l s out)
    (conde
     [succeed
      (fresh (a d d-out)
             (conso a d l)
             (conso a d-out out)
             (swappendo d s d-out))]
     [(nullo l) (== s out)])))

(define unwrap
  (λ (x)
    (cond
      [(pair? x) (unwrap (car x))]
      [else x])))

(define unwrapo-bad
  (λ (x out)
    (conde
     [(pairo x) (fresh (a)
                       (caro x a)
                       (unwrapo-bad a out))]
     [(== x out)])))

(define unwrapo
  (λ (x out)
    (conde
     [succeed (== x out)]
     [(fresh (a)
             (caro x a)
             (unwrapo a out))])))

(define flatten
  (λ (s)
    (cond
      [(null? s) '()]
      [(pair? s) (append
                  (flatten (car s))
                  (flatten (cdr s)))]
      [else (cons s '())])))

(define flatteno
  (λ (s out)
    (conde
     [(nullo s) (== '() out)]
     [(pairo s) (fresh (a d a-out d-out)
                       (conso a d s)
                       (flatteno a a-out)
                       (flatteno d d-out)
                       (appendo a-out d-out out))]
     [(conso s '() out)])))

(define flattenrevo
  (λ (s out)
    (conde
     [succeed (conso s '() out)]
     [(nullo s) (== '() out)]
     [(fresh (a d a-out d-out)
             (conso a d s)
             (flatteno a a-out)
             (flatteno d d-out)
             (appendo a-out d-out out))])))
