(load "Chapter-05-Def.scm")

;; ========= Panel 01 - 11 =========

;; [anyo]
;; Succeed when [g] succeed
;; Repeat the success of [g] any # of times
(define anyo
  (lambda (g)
    (conde
     [g succeed]
     [(anyo g)])))

;; [nevero]
;; This will recurse forever
;; [nevero] is a diverging point"?
(define nevero
  (anyo fail))

;; There is still ways to extract values
;; If we end unification early, never
;; reaching [nevero]
(run 1 (q)
     fail
     nevero)

;; [alwayso] always succeed in at least one way
(define alwayso
  (anyo succeed))

(run 1 (q)
     alwayso
     (== 't q))


;; [succeed] only succeed once
;; [alwayso] succeed arbitrary number of times

;; The 10 [#t]s came from 10 ways to satisfy [(anyo succed)]
;; by going deeper and deeper into the second condition in [conde]
(run 10 (q)
     alwayso
     (== #t q))
(run 10 (q)
     (== #t q)
     alwayso)

;; I just realized I might have been typing
;; ['t] instead of [#t] the whole time...

;; ========= Panel 12 - 18 =========

;; [salo] (Succeed At Least Once)
(define salo
  (lambda (g)
    (conde
     [succeed succeed]
     [g])))

;; Query once
(run 1 (q)
     (salo alwayso)
     (== #t q))
     
;; Interaction with [nevero]
(run 1 (q)
     (salo nevero)
     (== #t q))

#| The following examples will never finish computing

1. The second branch of [conde] in [salo]
never produce a satisfying unification

(run 2 (q)
     (salo nevero)
     (== #t q))

2. Although the first branch of [salo] succeed,
that path is blocked by the [fail] in the next goal
[salo] now tries [nevero], which never terminate

(run 1 (q)
     (salo nevero)
     fail
     (== #t q))

3. Every succeeding branch of [alwayso] is blocked
by the [fail]. Will keep try second branch

(run 1 (q)
     alwayso
     fail
     (== #t q))

4.
- We enter the first branch of the outer [conde]
- We succeed in unifying [q = #f]
- [alwayso] succeed
- Jump out of [conde]
- Fail to unify [#t] and [q = #f]
- Go back to try the other branch of [alwayso]
- But the association [q = #f] remains
- Will fail again, repeat

(run 1 (q)
     (conde
      [(== #f q) alwayso]
      [(anyo (== #t q))])
     (== #t q))
|#

;; ========= Panel 19 - =========
;; [condi]

