#lang racket

(require rackunit)

(define apply-env
  (λ (env y) (env y)))
(define apply-clos
  (λ (c a) (c a)))
(define extend-env
  (λ (x a env)
    (λ (y) (if (eqv? x y) a (apply-env env y)))))
(define make-clos
  (λ (x b env)
    (λ (a) (eval-expr 

;; Env :: 
;; Eval :: Expr -> Env -> Value
(define (eval-expr expr env) ;; env is treated as a function, that when given an identifier, output the associated value
  (match expr
    ;; Number
    [`,n #:when (number? n) n]
    ;; Variable
    [`,y #:when (symbol? y)
         (env y)]
    ;; Lambda abstraction
    [`(λ (,x) ,b)
     (λ (a)
       (eval-expr b (λ (y) (if (eqv? x y) a (env y)))))]
    ;; Function application
    [`(,rator ,rand) 
     ((eval-expr rator env)
      (eval-expr rand env))]

    ;; Here are the extensions we can make
    [`(* ,ne1 ,ne2)
     (* (eval-expr ne1 env) (eval-expr ne2 env))]
    ))

(define empty-env
  (λ (y) (error "env is empty")))
