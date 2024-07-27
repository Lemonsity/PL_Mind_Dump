#lang racket

(require "Racket-miniKanren/miniKanren/mk.rkt")
(require "Chapter-01-Def.rkt" "Chapter-02-Def.rkt")

(provide (all-defined-out))

(define listo
  (λ (l)
    (conde
     [(nullo l) succeed]
     [(pairo l) (fresh (d)
                       (cdro l d)
                       (listo d))]
     [fail])))
