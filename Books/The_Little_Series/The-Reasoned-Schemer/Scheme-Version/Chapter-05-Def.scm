(load "Chapter-04-Def.scm")

(define append-
  (lambda (l s)
    (cond
      [(null? l) s]
      [else (cons (car l)
                  (append (cdr l) s))])))

(define appendo1
  (lambda (l s out)
    (conde
     [(nullo l) (== s out)]
     [(fresh (a d d-out)
             
             (caro l a) ; Might be able to replace these two lines with
             (cdro l d) ; [(conso a d l)]
             
             (appendo1 d s d-out)
             (conso a d-out out))])))

(define appendo2
  (lambda (l s out)
    (conde
     [(nullo l) (== s out)]
     [(fresh (a d d-out)
             (conso a d l)
             (appendo2 d s d-out)
             (conso a d-out out))])))

(define appendo
  (lambda (l s out)
    (conde
     [(nullo l) (== s out)]
     [(fresh (a d d-out)
             (conso a d l)
             (conso a d-out out)
             (appendo d s d-out))])))

(define swappendo
  (lambda (l s out)
    (conde
     [succeed
      (fresh (a d d-out)
             (conso a d l)
             (conso a d-out out)
             (swappendo d s d-out))]
     [(nullo l) (== s out)])))

(define unwrap
  (lambda (x)
    (cond
      [(pair? x) (unwrap (car x))]
      [else x])))

(define unwrapo-bad
  (lambda (x out)
    (conde
     [(pairo x) (fresh (a)
                       (caro x a)
                       (unwrapo-bad a out))]
     [(== x out)])))

(define unwrapo
  (lambda (x out)
    (conde
     [succeed (== x out)]
     [(fresh (a)
             (caro x a)
             (unwrapo a out))])))

(define flatten
  (lambda (s)
    (cond
      [(null? s) '()]
      [(pair? s) (append
                  (flatten (car s))
                  (flatten (cdr s)))]
      [else (cons s '())])))

(define flatteno
  (lambda (s out)
    (conde
     [(nullo s) (== '() out)]
     [(pairo s) (fresh (a d a-out d-out)
                       (conso a d s)
                       (flatteno a a-out)
                       (flatteno d d-out)
                       (appendo a-out d-out out))]
     [(conso s '() out)])))

(define flattenrevo
  (lambda (s out)
    (conde
     [succeed (conso s '() out)]
     [(nullo s) (== '() out)]
     [(fresh (a d a-out d-out)
             (conso a d s)
             (flattenrevo a a-out)
             (flattenrevo d d-out)
             (appendo a-out d-out out))])))
