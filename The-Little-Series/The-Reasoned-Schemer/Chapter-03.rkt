#lang racket

(require "Chapter-02-Def.rkt")

;; ========= Panel 1-6 =========
;; [listo] definition

#| Notes on generalized translation
Suppose we have a function [test? : A -> Bool]
We can follow some general guideline to turn it into [testo]

We can replace all occurances of
- [#t] with [succeed]
- [#f] with [fail]
- function call [f?] with [fo], where [f?] is a boolean function

The ideas are:
- Truthness will be represented by [succeed]/[fail] of a goal
- [#t] will turn into [succeed], represent the current
substitution is a witness to why the test succeeded
- [#f] will turn into [fail], represent the current substitution
can no longer satisfy the test, hence a deadend
- Every boolean function call [f] will be turned into a goal,
which on [succeed] will "refine" the subsitution, on [fail]
will end attempt to unify 
|#
(define list?'
  (λ (l)
    (cond
      [(null? l) #t]
      [(pair? l) (list?' (cdr l))]
      [else #f])))

(define listo
  (λ (l)
    (conde
     [(nullo l) succeed]
     [(pairo l)
      ;; Here is a "CPS" translation of [(list?' (cdr l))]
      (fresh (d)
             (cdro l d)
             (listo d))]
     [fail])))

#| ========= The First Commandment =========

To transform a function whose value is a Boolean
into a function whose value is a goal, replace cond
with conde and unnest each question and answer.
Unnest the answer #t (or #f) by replacing it with #s
(or #u).

(I wrote the previous paragraph without reading the first commandment)
(So basically I wasted my time when it was right beneath me...)
|#

;; ========= Panel 7, 8, 9 =========
;; [x] associated with [_.0] anything
;; During unification of [listo],
;; [x] did not need to associate with a value
;; Thus it remained fresh, and reify to [_.0]
(run* (x)
      (listo '(a b x d)))

;; ========= Panel 10 - 15 =========
;; Limit to 1 association
(run 1 (x)
     (listo `(a b c . ,x)))
;; Limit to 5 association
(run 5 (x)
     (listo `(a b c . ,x)))
;; Each line of [conde] results in a goal
;; First first few elements are consumed by the [pairo]
;; line in [listo]
;; When arriving at [(listo `(c . ,x))], we are once
;; again forced down [(pairo ...)] path
;; However, now we arrive at [(listo x)], and b/c [x] is
;; fresh, there are suddenly two ways for the goal to
;; succeed:
;; - (nullo x), or
;; - (pairo x)
;; The first one gives the association [x = '()],
;; The second one generates infinitely more associations

;; ========= Panel 16 - 19 =========
;; [lol?/o] List of Lists
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
     [(fresh (a) ;; Unnesting
             (caro l a)
             (listo a))
      (fresh (d) ;; Unnesting
             (cdro l d)
             (lolo d))]
     [fail])))

;; ========= Panel 20 - 25 =========
;; Usage of [lolo]
;; The "base case" of [conde]
(run 1 (l)
     (lolo l))
;; Checking is it possible to succeed?
(run* (q)
      (fresh (x y)
             (lolo `((a b) (c ,x) (d ,y)))
             (== #t q)))
;; Checking is it possible to succeed?
(run 1 (q)
     (fresh (x)
            (lolo `((a b) (c d) . ,x))
            (== #t q)))
(run 5 (q) ;; Changed to 5
     (fresh (x)
            (lolo `((a b) (c d) . ,x))
            (== #t q)))
;; ^^ We will get a list of 5 [#t], because miniKanren
;; Finds 5 distinct associations for [x] that can
;; satisfy [lolo]
;; Each of which then proceed to [(== #t q)], and 
;; associates [q] with [#t]

;; To see the value [x] is associated:
(run 5 (x)
     (lolo `((a b) (c d) . ,x)))


;; ========= Panel 26 - 36 =========
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

;; I don't have too much to say about [twino]
;; It's similar to [listo] and [lolo]?

;; ========= Panel 37 - =========
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
