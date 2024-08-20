(load "Chapter-03-Def.scm")

(define mem
  (lambda (x l)
    (cond
      [(null? l) #f]
      [(eq-car? l x) l]
      [else (mem x (cdr l))])))

(define memo
  (lambda (x l out)
    (conde
     [(nullo l) fail]
     [(eq-caro l x) (== l out)]
     [(fresh (d)
             (cdro l d)
             (memo x d out))])))

(define rember
  (lambda (x l)
    (cond
      [(null? l) l] ;; or [(null? l) null]
      [(eq-car? l x) (cdr l)]
      [else (cons (car l)
                  (rember x (cdr l)))])))

(define rembero
  (lambda (x l out)
    (conde
     [(nullo l) (nullo out)]
     [(eq-caro l x) (cdro l out)]
     [(fresh (a d d-out)
             (conso a d l) ; destructing a pair
             (rembero x d d-out)
             (conso a d-out out))]))) ; constructing a pair

(define surpriseo
  (lambda (s)
    (rembero s '(a b c) '(a b c))))
