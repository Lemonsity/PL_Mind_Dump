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

;; ========= Panel 41 - 51 =========

;; Treat an s-exp as a tree
;; [unwrap] takes the left-most, deepest element
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

#| The problem with [unwrapo-bad] 
- Recall [conde] trys every branch, thus forall value [v], 
  [(unwrapo-bad v v)] will be satisfied
  + This is incorrect if [v = '(...)]
|#
(run* (out)
      (unwrapo-bad '(((pizza))) out))


#| More problems with [unwrapo-bad] 
The following two expression will not give a result in 
standard miniKanren with no interleaving

This is because we will continuely descend down the first
branch of [conde]

(The vanilla definition of miniKanren does interleaving, 
so it will still find a result)
|#
;; (run 1 (x)
;;       (unwrapo-bad x 'pizza))
;; (run 1 (x)
;;       (unwrapo-bad `((,x)) 'pizza))

;; ========= Panel 52 - 57 =========
;; Good [unwrapo]
;; We can put the base case as the first condition to
;; avoid infinite recursion
(define unwrapo
  (λ (x out)
    (conde
     [succeed (== x out)]
     [(fresh (a)
             (caro x a)
             (unwrapo a out))])))

;; We can now query without running into infinite recursion
(run 5 (x)
     (unwrapo x 'pizza))

;; Again, I don't think the result we got here is good
;; Because [ [succeed (== x out)] ] still makes every
;; [(unwrapo v v)] satisfiable
(run 5 (x)
     (unwrapo x '((pizza)) ))
;; Still, better than before

;; ========= Panel 58 - 70 =========
(define flatten
  (λ (s)
    (cond
      [(null? s) '()]
      [(pair? s) (append
                  (flatten (car s))
                  (flatten (cdr s)))]
      [else (cons s '())])))

;; This is a straight translation of [flatten]
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

;; These two gives different result than the book
;; I think it is because [conde] interleaves
(run 1 (out)
     (flatteno '((a b) c) out))
(run 1 (out)
     (flatteno '(a (b c)) out))

;; [flatteno] gives wrong result
(run* (out)
      (flatteno '((a b) c) out))
;; Though luckily, all can be flattened to the correct result

;; Simple [flatteno] can still give too many result
;; Many are bad
(run* (out)
      (flatteno '(a) out))
(let ([result-list (run* (out)
                         (flatteno '((a)) out))])
  `(,result-list . ,(length result-list)))

;; ========= Panel 71 - 80 (End) =========

;; The following will not give result
;; Too many candidate inputs can be flattened
;; (run* (s)
;;       (flatteno s '(a b c)))

(define flattenrevo
  (λ (s out)
    (conde
     [succeed (conso s '() out)]
     [(nullo s) (== '() out)]
     [(fresh (a d a-out d-out)
             (conso a d s)
             (flatteno a a-out)
             (flatteno d d-out)
             (appendo a d out))])))

;; I skipped [reverse], because our [conde] is not sequential
;; anyway, so we won't see the result

;; Eh, I don't think changing order will turn
;; infinite list into finite list
;; Which it doesn't. Following will not give value back
;; (run* (s)
;;       (flattenrevo s '(a b c)))

;; We can still query for the first couple
(run 1 (s)
     (flattenrevo s '(a b c)))
(run 2 (s)
     (flattenrevo s '(a b c)))

;; In book, this one will not find a 3rd value
;; Because when the 3rd branch recursively calls [flattenrevo]
;; All arguments are uninitialized
(run 3 (s)
     (flattenrevo s '(a b c)))
;; We might stand a better chance if we bring
;; the [(appendo a d out)] goal a bit earlier

(length
 (run* (out)
       (flattenrevo '((((a (((b))) c))) d) out)))
