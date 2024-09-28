(load "TheReasonedSchemer/mkextraforms.scm")
(load "TheReasonedSchemer/mk.scm")

(define teacupo
  (lambda (x)
    (conde
     [(== 'tea x) succeed]
     [(== 'cup x) succeed]
     [fail])))
