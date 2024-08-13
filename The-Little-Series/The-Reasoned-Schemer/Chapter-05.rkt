#lang racket

(require "Chapter-04-Def.rkt")

;; ========= Panel 01 - 19 =========
;; [append] and [appendo]

;; This is not necessarily correct, because [s] may not be list
(define append-
  (λ (l s)
    (cond
      [(null? l) s]
      [else (cons (car l)
                  (append (cdr l) s))])))

;; [appendo]
(define appendo1
  (λ (l s out)
    (conde
     [(nullo l) (== s out)]
     [(fresh (a d d-out)
             
             (caro l a) ; Might be able to replace these two lines with
             (cdro l d) ; [(conso a d l)]
             
             (appendo1 d s d-out)
             (conso a d-out out))])))

;; Basic FP usage
(run* (out)
      (appendo1 '(ice cream) '(tastes yummy) out))
;; Arbitrary tail
(run* (out)
      (fresh (s)
             (appendo1 '(ice cream sandwich) s out)))
;; Forcing [l] to be a list
;; This only returns the result for [l-tail = '()]
(run 1 (out)
     (fresh (l-tail)
            (appendo1 `(ice cream . ,l-tail) '(d t) out)))

;; Asking for a longer [l]
(run 3 (out)
     (fresh (l-tail)
             (appendo1 `(ice cream . ,l-tail) '(d t) out)))

;; Panel 15
;; As suspected, we can replaced the [caro, cdro] with a single [conso]
(define appendo2
  (λ (l s out)
    (conde
     [(nullo l) (== s out)]
     [(fresh (a d d-out)
             (conso a d l)
             (appendo2 d s d-out)
             (conso a d-out out))])))

;; Asking for longer [l]
(run 5 (out)
     (fresh (l-tail)            
            (appendo2 `(ice cream . ,l-tail) '(d t) out)))
;; Query for [l-tail] instead
(run 5 (l-tail)
     (fresh (out)
            (appendo2 `(ice cream . ,l-tail) '(d t) out)))

;; ========= Panel 20 - 30 =========
;; Querying for result and arguments
;; And uninitialized value issue

;; Sharing tail
(run 5 (out)
     (fresh (l-tail)
            (appendo2
             `(ice cream cake . ,l-tail)
             `(d t . ,l-tail)
             out)))

;; Tail of [s] does not matter
(run* (out)
      (fresh (s-tail)
             (appendo2 '(ice cream cake)
                      `(d t . ,s-tail)
                      out)))

;; Deduce argument from result
(run 6 (l)
      (fresh (s)
             (appendo2 l s '(ice cream cake with cookies))))
(run 6 (s)
      (fresh (l)
             (appendo2 l s '(ice cream cake with cookies))))

;; Slight modification can query both
(run 6 (q)
     (fresh (l s)
            (appendo2 l s '(ice cream cake with cookies))
            (== `(,l ,s)  q)))

;; Query for more elements than [(length list) + 1] will result in infinite search
;; (run 7 (q)
;;     (fresh (l s)
;;            (appendo2 l s '(ice cream cake with cookies))
;;            (== `(,l ,s)  q)))

(run 1 (q)
     (fresh (l s)
            (appendo2 l s '())
            (== `(,l ,s)  q)))
#| What happens when querying for too much
Consider the above term.
- Case [(nullo l) (== s out)] will be satisfied. [l, s] remains fresh,
  thus are binded to ['()] each
- Case [else] is now triggered
  * [a, d, d-out] are fresh, [out = '()]
  * [(conso a d l)] binds [l = (cons a d)]. b/c everything is still fresh
  * Now try the goal [(appendo2 d s d-out)]
    + Notice [d, s, d-out] are still fresh
    + We now try [(nullo d) (== s out)]
    + This binds [d = '()], [s = d-out]
    + Return to try satisfy [(conso a d-out out)] 
    + This is not satisfiable, because can't [(cons a d)] to get ['()]
    + Backtrack to the [else] case in the resurvie [appendo2] call
    + ...
We now entered a recursive case.
[d, s, d-out] will always be fresh.
We will keep hitting deadend during [(nullo l) ...] case
We will keep descending into recursion
But we will never find an answer
|#

;; ========= Panel 31, 32 =========
;; Order Matters
(define appendo
  (λ (l s out)
    (conde
     [(nullo l) (== s out)]
     [(fresh (a d d-out)
             (conso a d l)
             (conso a d-out out)
             (appendo d s d-out))])))

(run* (q)
      (fresh (l s)
             (appendo l s '(ice cream cake with cookies))
             (== `(,l ,s) q)))
;; The previous issue is addressed,
;; Before every recursive call, [(conso a d-out out)] consumes head of [out],
;; and set [d-out = (cdr out)]
;; Thus when making recursive call, [d-out] is always initialized with concrete value
;; Eventually we have [d-out = '()]
;; Then the goal [(conso a d-out out)] will fail, and end searching

;; ========= Panel 33 - 37 =========

;; Recursion makes [x] longer
(run 7 (x)
     (fresh (y z)
            (appendo x y z)))
;; [y, z] remained fresh, so reify to [_.0]
(run 7 (y)
     (fresh (x z)
            (appendo x y z)))
;; [x] has to be longer list, but [y] can be whatever
(run 7 (z)
     (fresh (x y)
            (appendo x y z)))

(run 7 (q)
     (fresh (x y z)
            (appendo x y z)
            (== `(,x ,y ,z) q)))

;; ========= Panel 38 - 40 =========
(define swappendo
  (λ (l s out)
    (conde
     [succeed
      (fresh (a d d-out)
             (conso a d l)
             (conso a d-out out)
             (swappendo d s d-out))]
     [(nullo l) (== s out)])))

;; Below will give no result
;; b/c recursive call on 3 fresh variables, so we make no progress
;; (run 1 (z)
;;     (fresh (x y)
;;            (swappendo x y z)))

;; Panel 40
;; I am ashamed that I don't know enough about Scheme macro...
;; I need to learn it

