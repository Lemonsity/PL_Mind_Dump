#lang racket

(require "Racket-miniKanren/miniKanren/mk.rkt")

(provide (all-defined-out))
(provide (all-from-out "Racket-miniKanren/miniKanren/mk.rkt"))

(define teacupo
  (lambda (x)
    (conde
     [(== 'tea x) succeed]
     [(== 'cup x) succeed]
     [fail])))
