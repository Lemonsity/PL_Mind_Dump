#lang racket

(require minikanren)

;; Panel 10
;; No unification can satisfy [fail]
(run* (q) fail)

;; Panel 11
;; Only (q = #t) satisfies [(== #t q)]
(run* (q)
      (== #t q)) 

;; Panel 12
;; [fail] can not be satisfied, thus whole goal cannot be satisfied
(run* (q)
      fail
      (== #t q))

;; Panel 13, 14
;; [succeed] always succeed, the second goal dictate outcome
(run* (q)
      succeed
      (== #t q))

;; Panel 15, 16
;; We can unify more than boolean
(run* (r)
      succeed
      (== 'corn r))

;; Panel 17
;; [fail] cannot be satisfied

;; Panel 18
;; Unifying to #f is still a succeed
;; succeed != #t
(run* (q)
      succeed
      (== #f q))

;; Panel 19, 20, 21
;; [let] can be used to set values
(let ([x #t])
  (== #f x)) ;; This will fail
;; ^^ vv are equivalent
((λ (x) (== #f x)) #t)

(let ([x #f])
  (== #f x)) ;; This will succeed

;; Panel 22
;; Force binding, equivalent to just [(run* (x) (== #t #f))]
;; (There might be some shadowing happening here)
(run* (x)
      (let ([x #f])
        (== #t x)))

;; Panel 23, 24, 25
;; The Law of Fresh
;; If [x] is fresh,
;; Then (== v x) succeed and associate [x] with [v]
;; Fresh variable always succeed in binding
;; x only introduced as intermediate value
;; Running only show what value [q] takes
;;         does not show what value [x] takes on
(run* (q)
      (fresh (x)
             (== #t x)
             (== #t q)))

;; Panel 26, 27
;; The Law of ==
;; [==] is symmetric
(run* (q)
      (fresh (x)
             (== x #t)
             (== q #t)))

;; Panel 28
;; Reifying
;; _.N is "similar" to **Most General Unifier**
(run* (x)
      succeed)

;; Panel 29
;; Shadowing
(run* (x)
      (let ([x #f]) ;; Shadow
        (fresh (x)  ;; Shadow again
               (== #t x)))) ;; [x] refers to the one introed by [fresh]

;; Panel 30, 31
;; Introducing multiple fresh variable
;; [x, y] are both general
(run* (r)
      (fresh (x y)
             (== (cons x (cons y '())) r)))

;; Panel 32, 33
(run* (r)
      (fresh (x)
             (let ([y x])
               (fresh (x) ;; This [x] is different from the previous [x]
                      (== (cons y (cons x (cons y '()))) r)))))

;; Panel 34, 35
;; Cannot unify to disjoint value
(run* (q)
      (== #f q)
      (== #t q))
(run* (q)
      (== #f q)
      (== #f q))

;; Panel 36, 37
;; Co-refer / Share
;; When one variable is associated with another, they co-refer/share
(run* (q)
      (fresh (x)
             (== x q)))

;; Panel 38, 39
;; The two expressions produce the same substitution
;; But because the order is diferent,
;; The logic behind the result is different too
(run* (q)
      (fresh (x)
             (== #t x) ;; x = #t
             (== x q))) ;; then q = x
(run* (q)
      (fresh (x)
             (== x q) ;; q = x
             (== #t x))) ;; x = #t
                         ;; So it must be q = #t also

;; Panel 40
;; All fresh variables are different
(run* (q)
      (fresh (x)
             (fresh (y)
                    (== (eq? x y) q))))
(run* (q)
      (fresh (x)
             (== (eq? x q) q)))

;; Panel 41, 42, 43
;; Normal [cond]
(cond
  [#f succeed]
  [else fail])

;; Panel 44, 45, 46
;; [conde]
;; [conde] behaves like [cond]
(conde
 [fail succeed]
 [fail])
(conde
 [fail fail]
 [succeed])
(conde
 [succeed succeed]
 [fail])

;; Panel 47
;; Getting multiple values out of conde
(run* (x)
      (conde
       [(== 'olive x) succeed]
       [(== 'oil x) succeed]
       [fail]))

;; Panel 48
;; The Law of conde
;; Try all lines
;; For each line
;;   - If succeed, append the substitution to the result stream
;;   - If fail, do nothing to the stream
;;   - Regardless of succeed/failed, try next line with refreshed variable

;; "e" stands for "every line"

;; Panel 49
;; Limited [run]
;; Second argument limits the size of stream
(run 1 (x)
     (conde
      [(== 'olive x) succeed]
      [(== 'oil x) succeed]
      [fail]))

;; Panel 50, 51
(run* (x)
      (conde
       [(== 'virgin x) fail]
       [(== 'olive x) succeed]
       [succeed succeed] ;; This creates a reified variable _.0
       [(== 'oil x) succeed]
       [fail]))

;; Panel 52
(run 2 (x) ;; result in list of 2 substitutions
     (conde
      [(== 'extra x) succeed]
      [(== 'virgin x) fail]
      [(== 'olive x) succeed]
      [(== 'oil x) succeed]
      [fail]))

;; Panel 53 skipped

;; Panel 54, 55
;; Each line in conde produce a substitution
;; Each substitution associate [r] with a list of 2 elements
(run* (r)
      (fresh (x y)
             (conde
              [(== 'split x) (== 'pea y)]
              [(== 'navy x) (== 'bean y)]
              [fail])
             (== (cons x (cons y '())) r)))

;; Panel 56
;; Wrapping conde
(define teacupo
  (lambda (x)
    (conde
     [(== 'tea x) succeed]
     [(== 'cup x) succeed]
     [fail])))

(run* (x)
      (teacupo x))

;; Panel 57
#|

On Goal:
A goal produces a stream of substitutions
[==, conde] are "forms" that produce goals

On [conde]
Each line of [conde] is a list of goals
Each line gets mapped to a stream of substitutions
[conde] returns the concatination of the streams
  produced by each line

Definition: Question & Answer
Question is the first goal of a line of [conde]
Answer   is the rest of the goals
Both Question and Answer need to succeed for the whole
  line to succeed
|#
(run* (r)
      (fresh (x y)
             (conde
              [(teacupo x) (== #t y) succeed]
              [(== #f x) (== #t y)]
              [fail])
             (== (cons x (cons y '())) r)))

;; Panel 58, 59
;; The following results in ['((_.0 _.1) (_.0 _.1))]
;; However, this does not mean the [car] of both
;; 2-elements lists are associated with the same value
(run* (r)
      (fresh (x y z)
             (conde
              [(== y x) (fresh (x) (== z x))]
              [(fresh (x) (== y x)) (== z x)]
              [fail])
             (== (cons y (cons z '())) r)))
;; The difference is demonstrated here
(run* (r)
      (fresh (x y z)
             (conde
              [(== y x) (fresh (x) (== z x))]
              [(fresh (x) (== y x)) (== z x)]
              [fail])
             (== #t x)
             (== (cons y (cons z '())) r)))

;; Panel 60
;; Goals are also values
(run* (q)
      (let ([a (== #t q)]
            [b (== #f q)])
        b))
;; The above is equivalent to:
(run* (q)
      (== #f q))
;; By considering the substitution

;; Panel 61
;; [==, fresh, conde] are all expressions
;; All of them produce Goal as value
(run* (q)
      (let ([a (== #t q)]
            [b (fresh (x)
                      (== x q)
                      (== #f x))]
            [c (conde
                [(== #t q) succeed]
                [(== #f q)])])
        b))
