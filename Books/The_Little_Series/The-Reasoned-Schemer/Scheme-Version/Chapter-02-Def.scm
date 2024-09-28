(load "Chapter-01-Def.scm")

(define caro
  (lambda (p a)
    (fresh (d)
           (== (cons a d) p))))

(define cdro
  (lambda (p d)
    (fresh (a)
           (== (cons a d) p))))

(define car& (lambda (p k) (k (car p))))
(define cdr& (lambda (p k) (k (cdr p))))

(define conso
  (lambda (a d p)
    (== (cons a d) p)))

(define nullo
  (lambda (l)
    (== '() l)))

(define eqo
  (lambda (x y)
    (== x y)))

(define pairo
  (lambda (p)
    (fresh (x y)
           (conso x y p))))

(define caro-
  (lambda (p a)
    (fresh (d)
           (conso a d p))))
(define cdro-
  (lambda (p d)
    (fresh (a)
           (conso a d p))))
(define pairo-
  (lambda (p)
    (fresh (a d)
           (conso a d p))))

