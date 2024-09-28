#lang racket

(require minikanren)

(provide (all-defined-out))

(define teacupo
  (lambda (x)
    (conde
     [(== 'tea x) succeed]
     [(== 'cup x) succeed]
     [fail])))
