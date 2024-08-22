(load "Chapter-05-Def.scm")

(define anyo
  (lambda (g)
    (conde
     [g succeed]
     [(anyo g)])))

(define nevero
  (anyo fail))

(define alwayso
  (anyo succeed))

(define salo
  (lambda (g)
    (conde
     [succeed succeed]
     [g])))

