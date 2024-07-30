#lang typed/racket

(: peirce (All (X) (-> (-> (-> X Nothing) X) X)))
(define peirce
  (λ (f) ; (X -> Nothing) -> X
    (call/cc
     #|
     call/cc takes a function that takes a continuation as argument
     here we have       ^                                     ^
                        |                                     |
           the annonymous lambda function                   the k
     
     From now on we denote the annonymous function as h, the argument it takes stays as k

     Recall there is an outer context surrounding (call/cc ...).
     For now
       1. We assume the outer context is expecting type X where (call/cc ...) is located.
       2. We don't care what the outer context will eventually evaluate to, might as well be Nothing
     Hence, the outer context as a continuation is of type X -> Nothing
                                                           ^       ^
                                                           |       |
                           (call/cc ...)'s type ------------       |
                                                                   |
                           return type of outer context ------------
     

     If k is never used during evaluation of h, then (call/cc ...) will return the value of the expression (h k)
     So (h k) better evaluate to type X, as (call/cc ...) will soon return it

     If k is applied to any argument during evaluation of h, then k takes over the outer context
     Many aspects of the program gets changed

     (call/cc ...) itself is of type X also, because it sits in a larger context that demands a term of type X where it is
     |#
     (λ ([k : (-> X Nothing)])
       (f k)))))


(: temp (Listof Number))
(define temp
  (cons 1
        (call/cc (λ ([k : (-> (Listof Number) (Listof Number))])
                   (cons 2
                         (k (cons 3 empty)))))))