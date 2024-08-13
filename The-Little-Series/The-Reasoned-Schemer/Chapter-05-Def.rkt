#lang racket

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
