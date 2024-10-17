#lang racket

(define-syntax my-macro
  (syntax-rules ()
    [(my-macro (<x> (<y>) ...)) (list 'a <x>)]
    [(my-macro (<x>))           (list 'b <x>)]
    [(my-macro (<x> <y> ...))   (list 'c <y> ...)]))
