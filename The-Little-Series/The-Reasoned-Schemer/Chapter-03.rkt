#lang racket

(require "Chapter-02-Def.rkt")

;; Panel 1-6
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

;; Panel 7
#| The First Commandment

To transform a function whose value is a Boolean
into a function whose value is a goal, replace cond
with conde and unnest each question and answer.
Unnest the answer #t (or #f) by replacing it with #s
(or #u).

(I wrote the previous paragraph without reading the first commandment)
(So basically I wasted my time when it was right beneath me...)
|#

