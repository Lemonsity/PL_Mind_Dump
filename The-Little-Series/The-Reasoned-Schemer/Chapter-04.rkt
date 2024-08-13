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
;; Impossibility
(run* (x)
      (memo 'tofu '(tofu e) `(peas ,x)))
;; Multiple solutions
;; 1st solution binds [x = 'tofu], and take 4-elements tail
;; 2nd solution for the existing ['tofu], take 2-elements tail
(run* (out)
      (fresh (x)
             (memo 'tofu `(a b ,x d tofu e) out)))

;; Arbitrary tail
(run 12 (z)
     (fresh (u)
            (memo 'tofu `(a b tofu d tofu e . ,z) u)))
#|
'(_.0 b/c match first existing ['tofu], [z] can be anything
  _.0 b/c match second existing ['tofu], [z] can be anything
  (tofu . _.0) b/c [z] has to contain ['tofu], this case in [car]
  (_.0 tofu . _.1) b/c [z] has to contain [tofu], this case in [cadr]
  (_.0 _.1 tofu . _.2) b/c [z] has to contain [tofu], this case in [caddr]
  (_.0 _.1 _.2 tofu . _.3) ...
  (_.0 _.1 _.2 _.3 tofu . _.4)
  (_.0 _.1 _.2 _.3 _.4 tofu . _.5)
  (_.0 _.1 _.2 _.3 _.4 _.5 tofu . _.6)
  (_.0 _.1 _.2 _.3 _.4 _.5 _.6 tofu . _.7)
  (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 tofu . _.8)
  (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8 tofu . _.9))
|#

;; I am not rewriting [memo]
;; I am an arrogant bastard and explicitly write out fail case

;; ========= Panel 22 - 29 =========
;; Remove the first instance of [x]
(define rember
  (λ (x l)
    (cond
      [(null? l) l] ;; or [(null? l) null]
      [(eq-car? l x) (cdr l)]
      [else (cons (car l)
                  (rember x (cdr l)))])))

(define rembero
  (λ (x l out)
    (conde
     [(nullo l) (nullo out)]
     [(eq-caro l x) (cdro l out)]
     [(fresh (d-out)
             (fresh (d)
                    (cdro l d)
                    (rembero x d d-out))
             (fresh (a)
                    (caro l a)
                    (conso a d-out out)))])))
#| My solution of the [else] condition
     [(fresh (a d d-out)
             (caro l a)
             (cdro l d)
             (rembero x d d-out)
             (conso a d-out out))]))))

The official solution does a better job at illustrating CPS translation
|#

#| Alternative [else] clause
     [(fresh (a d d-out)
             (conso a d l) ; destructing a pair
             (rembero x d d-out)
             (conso a d-out out))]))) ; building a pair 
|#

;; ========= Panel 30 - 48 =========
;; Associate then remove
;; Associate [y = 'peas] then remove [y]
(run 1 (out)
     (fresh (y)
            (rembero 'peas `(a b ,y d peas e) out)))

;; A compliated behaviour:
(run* (out)
      (fresh (y z)
             (rembero y `(1 2 ,y 4 ,z 6 7) out)))
#| Explanation
'((2 1 4 _.0 6 7)       assoc [y = 1], then remove 1st element (so there is still a [1] left)
  (1 2 4 _.0 6 7)       assoc [y = 2], then remove 2nd element (the 2 remain b/c [y] remained)
  (1 2 4 _.0 6 7)       reify [y], then remove [y]
  (1 2 4 _.0 6 7)       assoc [y = 4], then remove the 4th element (but b/c [y = 4], we got
                        still got a [4] in the resulting list
                        Arguably not the correct solution, but we don't have [=/=] yet
  (1 2 _.0 4 6 7)       assoc [y = z], remove [z]
  (1 2 6 4 _.0 7)       assoc [y = 6], remove the 6th element
                        same issue as [y = 4] case
  (1 2 7 4 _.0 6)       assoc [y = 7], remove the 7th element
                        same issue as [y = 4] case
  (1 2 _.0 4 _.1 6 7))  [y] matches nothing, remove nothing
|#


;; ========= Panel 49 - 56 =========
(run* (r)
      (fresh (y z)
             (rembero y `(,y d ,z e) `(,y d e))
             (== `(,y ,z) r)))
#| Explanation
'((d d)      assoc [y = 'd], remove the 1st element
  (d d)      assoc [y = 'd], remove the 2nd element
             Technically not correct, because we skipped 1st element
  (_.0 _.0)  assoc [y = z], remove the 3rd element
  (e e))     assoc [y = 'e], then assoc [z = 'e], remove the 4th element
             Technically not correct, because should remove 3rd element [z]
|#

;; ========= Panel 57 - 67 =========
(run 13 (w)
     (fresh (y z out)
            (rembero y `(a b ,y d ,z . ,w) out)))
;; The first 5 values are from removing the first 5 elements of the list
;; The rest are generated with [(rembero y w out)]
;; Details omitted

;; ========= Panel 68 - 72 (End) =========
;; Broken definition
;; Here we see how having fresh variable may give surprising result
(define surpriseo
  (λ (s)
    (rembero s '(a b c) '(a b c))))

;; This makes sense
(run* (r)
      (== 'd r)
      (surpriseo r))

;; We see calling [surpriseo] leaves [r] fresh
(run* (r)
      (surpriseo r))

;; [r] remained fresh means we can then associate [r = 'a]
(run* (r)
      (surpriseo r)
      (== 'a r))
;; But this makes no sense, because now going back
;; [(surpriseo 'a '(a b c) '(a b c))] is not satisfiable

;; Similarly, the line below succeed, but it makes so sense
(run* (r)
      (== 'a r)
      (surpriseo r))


            
