(load "Chapter-02-Def.scm")

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
(define list?-
  (lambda (l)
    (cond
      [(null? l) #t]
      [(pair? l) (list?' (cdr l))]
      [else #f])))

(define listo
  (lambda (l)
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
  (lambda (l)
    (cond
      [(null? l) #t]
      [(list? (car l)) (lol? (cdr l))]
      [else #f])))

(define lolo
  (lambda (l)
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
  (lambda (p)
    (fresh (a d)
           (conso a d p)
           ;; vv Make sure tail is one-element long
           (conso a '() d))))

(define twino-
  (lambda (p)
    (fresh (x)
           (== `(,x ,x) p))))

;; I don't have too much to say about [twino]
;; It's similar to [listo] and [lolo]?

;; ========= Panel 37 - 44 =========
(define loto
  (lambda (l)
    (conde
     [(nullo l) succeed]
     [(fresh (a)
             (caro l a)
             (twino a))
      (fresh (d)
             (cdro l d)
             (loto d))]
     [fail])))

(run 1 (q)
     (loto `((g g) . ,q)))
;; Query for 5 answers gives us 5 lists of diff length
(run 5 (q)
     (loto `((g g) . ,q)))

;; ========= Panel 45 - 47 =========
(run 5 (r)
     (fresh (w x y z)
            ;; [loto] creates following association
            ;; + [w = 'e]
            ;; + [x = y]
            ;; + [z] is a list of twin
            (loto `((g g) (e ,w) (,x ,y) . ,z))
            ;; [r] will have following association
            ;; [r = `(e (_.0 _.0) <some-loto>)]
            (== `(,w (,x ,y) ,z) r)))

;; ========= Panel 48 - 50 =========
;; Higher Order Relation?
;; Similar to higher order function
(define listofo
  (lambda (predo l)
    (conde
     [(nullo l) succeed]
     [(fresh (a)
             (caro l a)
             (predo a))
      (fresh (d)
             (cdro l d)
             (listofo predo d))]
     [fail])))

;; Rewrite [loto] with [listofo]
(define loto-
  (lambda (l)
    (listofo twino l)))

;; ========= Panel 51 - 65 =========
(define eq-car?
  (lambda (l x)
    (eq? (car l) x)))
(define member?
  (lambda (x l)
    (cond
      [(null? l) #f]
      [(eq-car? l x) #t]
      [else (member? x (cdr l))])))

(define eq-caro
  (lambda (l x)
    (caro l x)))

;; WARNING: [membero] actually works on pair too, not just lists
(define membero
  (lambda (x l)
    (conde
     ;; The first condition is not necessary
     ;; If a line is guaranteed to fail,
     ;; Then it is not necessary
     [(nullo l) fail]
     [(eq-caro l x) succeed]
     [(fresh (d)
             (cdro l d)
             (membero x d))])))

;; With the way we defined [membero]
;; We will always query the first element first
(run 1 (item)
     (membero item '(hummus with pita)))
(run 1 (item)
     (membero item '(with pita)))
(run 1 (item)
     (membero item '(pita)))
;; No item can be a member of null list
(run* (item)
      (membero item '()))
;; We can query more from a longer list
(run 3 (item)
     (membero item '(hummus with pita)))
;; But can only get as many answer as the length of list
(run* (item)
      (membero item '(hummus with pita with hummus)))

;; Panel 64, 65
;; In fact, [(run* (item) (membero item l)) = l]
(define identity-
  (lambda (l)
    (run* (item)
          (membero item l))))
       
;; ========= Panel 66 - 72 =========
;; Filling in gaps

;; Forcing [x] to be a value
;; Because other member of the list cannot be associated
;; with ['e]
(run* (x)
      (membero 'e `(pasta ,x fagioli)))

;; Order matters
;; vv binds [x] to [_.0]
(run 1 (x)
     (membero 'e `(pasta e ,x fagioli)))
;; vv binds [x] to ['e]
(run 1 (x)
     (membero 'e `(pasta ,x e fagioli)))

#|
In the first one, we ran into this condition:
(membero 'e `(e ,x fagioli))
-> (eq-caro `(e ,x fagioli) 'e)   b/c attempt this case
-> (caro `(e ,x fagioli) 'e)      
-> (fresh (d) (== (cons 'e d) `(e ,x fagioli))))
At this point, we will bind [d = `(,x fagioli)],
[x] remains fresh at this point, and will be reified

In the second one, we ran ito this condition:
(membero 'e `(,x e fagioli))
-> (eq-caro `(,x e fagioli) 'e)   b/c attempt this case
-> (caro `(,x e fagioli) 'e)      
-> (fresh (d) (== (cons 'e d) `(,x e fagioli))))
At this point, we will bind:
+ ['e = x]
+ [d = `(e fagioli)]
[x] is no longer fresh. [x] is now binded to ['e]
|#

;; The following example illustrates the containment of
;; ['e] can appear at different point of the list,
;; Which let other fresh variables have more general binding
(run* (r)
      (fresh (x y)
             (membero 'e `(pasta ,x fagioli ,y))
             (== `(,x ,y) r)))


;; ========= Panel 73 - 77 =========
(run 1 (l)
     (membero 'tofu l))

;; WARNING: The following will never terminate,
;; b/c there are infinite candidate for [l] that will
;; satisfy the relation
;; (run* (l)
;;       (membero 'tofu l))

;; We can get a hint of the infinite list generate
;; by the above commented code
(run 5 (l)
     (membero 'tofu l))

;; ========= Panel 78 - 94 =========
;; Membership of Proper List

;; Following is my attempt
;; It is bad, b/c it only finds lists with [x] as head
;; [(listo d)] have infinite solutions
;; Thus block the path where [x] appear in the tail
(define pmembero-bad
  (lambda (x l)
    (conde
     [(nullo l) fail] ;; Once again, this is redundant
     [(eq-caro l x) (fresh (d)
                           (cdro l d)
                           (listo d))]
     [(fresh (d)
             (cdro l d)
             (pmembero-bad x d))])))
;; However, the previous version will work with the
;; Latest miniKanren, because the built in interleave

(run 10 (l)
     (pmembero-bad 'tofu l))

;; Following is the "Official Solution"
(define pmembero
  (lambda (x l)
    (conde
     [(eq-caro l x) (fresh (a d)
                           (cdro l `(,a . ,d)))]
     [(eq-caro l x) (cdro l '())]
     [(fresh (d)
             (cdro l d)
             (pmembero x d))])))
;; However I think it still have flaws,
;; As demonstrated by the following example
(run 1 (q)
     (pmembero 'e '(e . (a . (b . c))))
     (== q #t))
;; The official solution also cannot generate
;; arbitrary tail length

(run 12 (l)
     (pmembero 'tofu l))

;; ========= My Attempt at a Prolog-style [pmembero] =========
;; I am encoding length with another list
;; 0 := '()
;; 1 := '( () )
;; 2 := '( () () ) ...

(define length-0 '())
(define length-1 '(()))
(define length-2 '(() ()))
(define length-3 '(() () ()))

(define my-lengtho
  (lambda (l len) 
    (conde
     [(nullo l) (== len '())]
     [(fresh (d sublen)
             (cdro l d)
             (conso '() sublen len)
             (my-lengtho d sublen))])))


(define my-pmembero-helper
  (lambda (x l l-len-sub1)
    (conde
     [(nullo l) fail]
     [(eq-caro l x) (fresh (d)                           
                           (cdro l d)
                           (my-lengtho d l-len-sub1))]
     [(fresh (d d-len-sub1)
             (cdro l d)
             (conso '() d-len-sub1 l-len-sub1)
             (my-pmembero-helper x d d-len-sub1))])))

(define my-pmembero-start-with
  (lambda (x l start-len)
    (conde
     [(my-pmembero-helper x l start-len) succeed]
     [(my-pmembero-start-with x l (cons '() start-len))])))

(define my-pmembero
  (lambda (x l)
    (my-pmembero-start-with x l length-0)))

;; I think my version of [my-pmembero] have a better chance
;; of being translated to Prolog,
;; B/c Prolog always tries to expand the first rule that
;; matches the requirement

;; ========= Panel 95 - 101 (End) =========
;; Misc

;; Get first element in a list
(define first-value
  (lambda (l)
    (run 1 (y)
         (membero y l))))

(first-value '(pasta e fagioli))

;; Changing order of [conde] changes output
(define memberrevo
  (lambda (x l)
    (conde
     [(nullo l) fail]
     ;; We now explore the path where [x] is in the tail first
     ;; Which will change order of output stream
     [succeed (fresh (d)
                     (cdro l d)
                     (memberrevo x d))]
     [(eq-caro l x)])))

(run* (x) (membero x '(pasta e fagioli)))
(run* (x) (memberrevo x '(pasta e fagioli)))

(define reverse-list
  (lambda (l)
    (run* (y)
          (memberrevo y l))))
