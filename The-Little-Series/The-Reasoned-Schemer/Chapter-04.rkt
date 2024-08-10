#lang racket

(require "Chapter-03-Def.rkt")

;; ========= Panel 1 - 6 =========
;; Review
(define mem
  (λ (x l)
    (cond
      [(null? l) #f]
      [(eq-car? l x) l]
      [else (mem x (cdr l))])))

;; ========= Panel 7, 8 =========
;; [memo]
(define memo
  (λ (x l out)
    (conde
     [(nullo l) fail]
     [(eq-caro l x) (== l out)]
     [(fresh (d)
             (cdro l d)
             (memo x d out))])))

#| The Second Commandment

To transform a function whose value is not a
Boolean to a function whose value is a goal,
add an extra argument to hold its value, replace
[cond] with [conde], and unnest each question
and answer.

We have already seen similar cases like
- [car] -> [caro]
- [cdr] -> [cdro]
- [cons] -> [conso]
|#

;; ========= Panel 9 - 21 =========
;; Using [memo]

;; This is using relation as a function
(run 1 (out)
     (memo 'tofu '(a b tofu d tofu e) out))
;; Here the first possible chance to succeed
;; is if [x = 'tofu]. The return value
;; ['(tofu d tofu e)] reflects that
(run 1 (out)
     (fresh (x)
            (memo 'tofu `(a b ,x d tofu e) out)))

;; Under the hood, [r] will be binded to ['a], ['b]
;; during computation process. But those two paths
;; lead to the goals:
;; - [(== '(a b tofu d tofu e) '(tofu d tofu e))]
;; - [(== '(b tofu d tofu e) '(tofu d tofu e))]
;; Both of which fails, which then leads to [r = 'tofu]
(run* (r)
      (memo r '(a b tofu d tofu e) '(tofu d tofu e)))

;; Trivially true
(run* (q)
      (memo 'tofu '(tofu e) '(tofu e))
      (== q #t))
;; Trivially false
(run* (q)
      (memo 'tofu '(tofu e) '(tofu))
      (== q #t))

;; Panel 15 TODO
