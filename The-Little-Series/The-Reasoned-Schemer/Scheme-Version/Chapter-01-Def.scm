(load "CodeFromTheReasonedSchemer2ndEd/trs2-impl.scm")

(define teacupo
  (lambda (x)
    (conde
     [(== 'tea x) succeed]
     [(== 'cup x) succeed]
     [fail])))
