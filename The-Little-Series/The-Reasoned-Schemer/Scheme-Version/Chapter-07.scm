(load "Chapter-06-Def.scm")

;; ========= Panel 01 - 14 =========
;; Implementing arithmetic related things

(define bit-xoro
  (lambda (x y r)
    (conde
     [(== 0 x) (== 0 y) (== 0 r)]
     [(== 1 x) (== 0 y) (== 1 r)]
     [(== 0 x) (== 1 y) (== 1 r)]
     [(== 1 x) (== 1 y) (== 0 r)]
     [fail])))

(run* (s)
      (fresh (x y)
	     (bit-xoro x y 0)
	     (== `(,x ,y) s)))
(run* (s)
      (fresh (x y)
	     (bit-xoro x y 1)
	     (== `(,x ,y) s)))
(run* (s)
      (fresh (x y r)
	     (bit-xoro x y r)
	     (== `(,x ,y ,r) s)))
     
(define bit-ando
  (lambda (x y r)
    (conde
     [(== 0 x) (== 0 y) (== 0 r)]
     [(== 1 x) (== 0 y) (== 0 r)]
     [(== 0 x) (== 1 y) (== 0 r)]
     [(== 1 x) (== 1 y) (== 1 r)]
     [fail])))

(run* (s)
      (fresh (x y)
	     (bit-ando x y 1)
	     (== `(,x ,y) s)))

;; [half-addero]
;; [c] is carry, [r] is least significant digit
;; x + y = r + 2 * c
(define half-addero
  (lambda (x y r c)
    (all
     (bit-xoro x y r)
     (bit-ando x y c))))

(run* (s)
      (half-addero 1 1 s 1))

;; [bit-xoro] when given a substitution, generates
;; a stream of 4 substitutions
;; Each substitution from [bit-xoro] is then fed
;; into [bit-ando], which only extends substitution
;; by associating [c] with a value
(run* (s)
      (fresh (x y r c)
	     (half-addero x y r c)
	     (== `(,x ,y ,r ,c) s)))


;; No infinite stream anywhere, [all] or [alli] both
;; should work

;; ========= Panel 15 - 18 =========

(define full-addero
  (lambda (b x y r c)
    (fresh (w xy wz)
	   (half-addero x y w xy) ; [w] is least sig, [xy] is most sig
	   (half-addero w b r wz) ; [r] is least sig
	   (bit-xoro xy wz c)))) ; the numbers checks out

(run* (s)
      (fresh (r c)
	     (full-addero 0 1 1 r c)
	     (== `(,r ,c) s)))       ; The order is reversed from nornal writing
(run* (s)
      (fresh (r c)
	     (full-addero 1 1 1 r c)
	     (== `(,r ,c) s)))       

;; Generate all possible [full-addero] relationships
(run* (s)
      (fresh (b x y r c)
	     (full-addero b x y r c)
	     (== `(,b ,x ,y ,r ,c) s)))

;; ========= Panel 19 - 79 =========
;; numbers

;; Represent numbers as binary list
;; With least significant digit at front
;; 0 = '()
;; 1 = '(1)
;; 2 = '(0 1)
;; 3 = '(1 1)
;; 4 = '(0 0 1) ...

(define build-num
  (lambda (n)
    (cond
      [(zero? n) '()]
      [(and (not (zero? n)) (even? n))
       (cons 0
         (build-num (quotient n 2)))]
      [(odd? n)
       (cons 1
         (build-num (quotient (- n 1) 2)))])))

;; [build-num]'s conditions have "non-overlapping property"
;; Thus we can rearrange them in any order

;; There is a lot about what numbers represents
;; I skipped them

;; ========= Panel 80 - 95 =========

;; [poso] positive?
;; Notice definition have the same meaning as [pairo]
;; Becareful when use, have to be numbers
(define poso
  (lambda (n)
    (fresh (a d)
	   (== `(,a . ,d) n))))

(run* (q)
      (poso '())
      (== #t q)) ; gives ['()]
(run* (q)
      (poso '(1))
      (== #t q)) ; gives ['(#t)]
(run* (q)
      (poso '(0 1 1))
      (== #t q)) ; gives ['(#t)]

;; [>1o], greater than 1
(define >1o
  (lambda (n)
    (fresh (a ad dd)
	   (== `(,a ,ad . ,dd) n))))

;; [n-repersentative] is the prefix sublist
;; up and including the rightmost [1]
;; If there are no [1], return empty list
;; Fresh variables do not count as [1]

;; (n-representative '(0 1 1)           ) = '(0 1 1)
;; (n-representative `(0 ,x 1 0 ,y . ,z)) = `(0 ,x 1)
;; (n-representative `(0 0 ,y . ,z)     ) = '()
;; (n-representative z                  ) = '()

;; ========= Panel 96 - 100 =========
;; Taste of [addero]
;; And Ground value

#| Ground Value
a "Ground Value" contains no variable

['(_.0 () _.0)] is not ground value, b/c the [_.0] represents variable
|#

;; ========= Panel 101 - 127 =========
(define addero
  (lambda (d n m r)
    (condi
     [(== 0 d) (== '() m) (== n r)]
     [(== 0 d) (== '() n) (== m r)
      (poso m)]
     [(== 1 d) (== '() m) (addero 0 n '(1) r)]
     [(== 1 d) (== '() n) (poso m)
      (addero 0 '(1) m r)]
     [(== '(1) n) (== '(1) m) (fresh (a c)
				     (== `(,a ,c) r)
				     (full-addero d 1 1 a c))]
     [(== '(1) n) (gen-addero d n m r)]
     [(== '(1) m) (>1o n) (>1o r) (addero d '(1) n r)]
     [(>1o n) (gen-addero d n m r)]
     [fail])))

;; [gen-addero]
;; d + n + m = r, provided n > 0 and  m, r > 1
(define gen-addero
  (lambda (d n m r)
    (fresh (a b c e x y z)
	   (== `(,a . ,x) n)
	   (== `(,b . ,y) m) (poso y)
	   (== `(,c . ,z) r) (poso z)
	   (alli
	    (full-addero d a b c e)
	    (addero e x y z)))))
     
(run 22 (s)
     (fresh (x y r)
	    (addero 0 x y r)
	    (== `(,x ,y ,r) s)))

#| Nonground Value Property
The nonground values have the property:
- [(width r) = max (width x) (width y)]
- [r] ends in [_.0 . _.1]                  (except one)
- Wider of [x] and [y] ends in [_.0 . _.1] (except one)
- [(n-representative r) = (+ (n-representative x)
                             (n-representative y))]
- In essense, these values shows there are no more carrying
  over, so the tail remains as [_.0 . _.1]
|#

#| Ground Values Property
- No reify variable
- [(width r) = 1 + (max (width x) (width y))]

In essense, these additions causes a carry
We cannot leave the tail as arbitrary, because we don't
know if the carry bit will affect the tail
|#

#| Nonoverlapping

We cannot substitute any values for [_.0, _.1] to any
unground value to obtain a grounded value

This is because of the nonoverlapping property
|#

(run* (s)
      (fresh (x y)
	     (addero 0 x y '(1 0 1))
	     (== `(,x ,y) s)))

;; ========= Panel 128 - 133 (End) =========
;; Actual arithmetic
(define +o
  (lambda (n m k)
    (addero 0 n m k)))

(define -o
  (lambda (n m k)
    (+o m k n)))

(run* (s)
      (fresh (x y)
	     (+o x y '(1 0 1))
	     (== `(,x ,y) s)))
(run* (y)
      (+o '(0 0 1) y '(1)))

(run* (diff)
      (-o '(0 0 0 1) '(1 0 1) diff))
(run* (diff)
      (-o '(1 0 1) '(1 0 1) diff))
(run* (diff)
      (-o '(0 0 1) '(0 0 0 1) diff))
