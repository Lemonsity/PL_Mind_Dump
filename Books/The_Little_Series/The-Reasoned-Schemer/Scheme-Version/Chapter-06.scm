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
that path is blocked by the [fail] in the next goal.
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

;; ========= Panel 19 - 23 =========
;; [condi]
;; Interleaving answers

;; This gives ['(#t)] b/c [condi] tries the second
;; branch after the first branch failed to unify with
;; the final [(== #t q)] goal
(run 1 (q)
     (condi
      [(== #f q) alwayso]
      [(== #t q)])
     (== #t q))

;; The following still gives no value
;; b/c we used up the one goal from the second branch
;; so [condi] can only try the first branch
;; (run 2 (q)
;;      (condi
;;       [(== #f q) alwayso]
;;       [(== #t q)])
;;      (== #t q))

;; We will get ['(#t #t #t #t #t)],
;; But all substitutions came from the second branch
;; We hit deadend 5 time in the first branch
(run 5 (q)
     (condi
      [( == #f q) alwayso]
      [(anyo (== #t q))])
     (== #t q))
      

#| The Law of [condi]
[condi] behaves liek [conde], but interleaves the values
|#

;; ========= Panel 24 - 30 =========

;; Demonstrate interleaving
(run 5 (q)
     (condi
      [(teacupo q) succeed]
      [(== #f q) succeed]
      [fail]))

;; Explicit comparison between [conde] & [condi]
;; With [conde]:
;; (run 5 (q)
;;      (conde                ; <--- watch this line
;;       [(== #f q) alwayso]
;;       [(== #t q) alwayso]
;;       [fail])
;;      (== #t q))

;; With [condi]:     
(run 5 (q)
     (condi                ; <--- watch this line
      [(== #f q) alwayso]
      [(== #t q) alwayso]
      [fail])
     (== #t q))


;; [condi] can get stuck too...
;; With [conde]
(run 5 (q)
     (conde            ; <--- watch this line
      [alwayso succeed]
      [nevero])
     (== #t q))

;; With [condi], we get one substitution from 1st condition
;; We then interleave with second condition
;; The [nevero] in 2nd condition blocks furthur computation
;; (run 5 (q)
;;     (condi            ; <--- watch this line
;;      [alwayso succeed]
;;      [nevero])
;;     (== #t q))

;; ========= Panel 31 - 39 (End) =========
;; [all] and [alli]

;; [all] requires all goals to succeed
;; It unify in the order of appearance

;; The following won't produce result
;; B/c [all] cannot to back one step to update the
;; substitution produced by [conde] after moving-on
;; to [alwayso]
;; (run 1 (q)
;;      (all
;;       (conde
;;        [(== #f q) succeed]
;;        [(== #t q)])
;;       alwayso)
;;      (== #t q))

;; But this gives result
(run 3 (q)
     (alli
      (conde
       [(== #f q) succeed]
       [(== #t q)])
      alwayso)
     (== #t q))

;; Here we see the ['(#t #t #t)] from the prev expression
;; was interleaved between [#f]s
(run 10 (q)
     (alli
      (conde
       [(== #f q) succeed]
       [(== #t q)])
      alwayso))

;; We can change the order of [conde] also
(run 3 (q)
     (alli
      (conde
       [(== #t q) succeed]
       [(== #f q)])
      alwayso)
     (== #t q))

#| Explaining [alli] (and some related forms)

[alli]   : (Goal, ..., Goal) -> Goal
[bindi]  : (Stream, Goal)    -> Stream
[mplusi] : (Stream, Stream)  -> Stream

Some ideas on how [alli] works:
- [alli] is a goal
  - Input : a substitution [s]
  - Output: a stream of substitutions
- [alli] gets a stream of subsitutions from the first goal
- [alli] use the stream of substitutions, and "refine" it using the remaining goals

- [bindi],
  - Input : a stream of substitutions [a-inf], a goal [g]
  - Output: a stream of substitutions
- For each substitution [a] in [a-inf]
  - [bindi] gets a new stream of substitutions via [(g a)]
- [bindi] then interleaves the streams using [mplusi]

Some final comments:
- [all, alli] may contain many goals [g1, g2, ...]
- There may be many substitutions [a1, a2, ...] that satisfies [g1]
- [all] will take [a1] as a starting point, and generate a stream
  of substitutions that will satisfy [g2, ...]
  - If the stream generated by [a1] runs out, [all] tries to 
    generate more with [a2]
  - Repeat
- Using each substitution [a1, a2, ...] as the starting point,
  [alli] will generate a stream of substitutions that will satisfy
  [g2, ...]
- [alli] then interleaves the streams

|#

;; Just like [conde/i], [alli] can get stuck too
(run 3 (q)
     (all                ; <-- watch this line
      (conde
       [succeed succeed]
       [nevero])
      alwayso)
     (== #t q))

;; This get stuck, b/c interleaving cause us to
;; run into [nevero]
;; (run 3 (q)
;;      (alli               ; <-- watch this line
;;       (conde
;;        [succeed succeed]
;;        [nevero])
;;       alwayso)
;;      (== #t q))
       
