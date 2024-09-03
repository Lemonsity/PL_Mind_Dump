(load "Chapter-07-Def.scm")

;; ========= Panel 01 - 25 =========
;; *o
(define *o
  (lambda (n m p)
    (condi 		                ; We are using [condi] here because there are multiple recursive cases
					; If we don't use [condi], we may dig infinitely deep into one case
     [(== '() n) (== '() p)]            ; first value is 0
     [(poso n) (== '() m) (== '() p)]   ; first value is nonzero, second value is zero
     [(== '(1) n) (poso m) (== m p)]    ; first value is 1, second value is nonzero
     [(>1o n) (== '(1) m) (== n p)]     ; first value is > 1, second value is 1
                                        ; I don't know if this is necessary
     [(fresh (x z)
	     (== `(0 . ,x) n) (poso x) ; if [n] is even
	     (== `(0 . ,z) p) (poso z) ; the surely [p] is even too
	     (>1o m)
	     (*o x m z))] ; just a shift
     [(fresh (x y)
	     (== `(1 . ,x) n) (poso x)
	     (== `(0 . ,y) m) (poso m) ;; if [m] is even
	     (*o m n p))] ; Then reduce the problem to simpler case of shift
                          ; since multiply is commutative
     [(fresh (x y)
	     (== `(1 . ,x) n) (poso x)
	     (== `(1 . ,y) m) (poso y)
	     (odd-*o x n m p))]
     [fail])))

(define odd-*o
  (lambda (x n m p)
    (fresh (q)
	   (bound-*o q p n m)
	   ;; Ignoring [bound-*o] and looking at just the two lines below
	   ;; Knowing [x = (n-1)/2], we know
	   ;; - [q = (n-1)/2 * m]
	   ;; - [p = q + m = (n-1)/2 * m + m], which is the correct answer when [n] is odd
	   (*o x m q)
	   (+o q m p))))

;; [bound-*o]
;; Force the sum bits of [n, m] to be less than the bits of [p]
;; (I think this is a confusing way to do it
(define bound-*o
  (lambda (q p n m)
    (conde
     [(nullo q) (pairo p)]
     [(fresh (x y z)
	     (cdro q x)
	     (cdro p y)
	     (condi
	      [(nullo n) (cdro m z)
	                 (bound-*o x y z '())]
	      [(cdro n z) (bound-*o x y z m)]))])))

;; ========= Panel 26 - 33 =========
(define =lo
  (lambda (n m)
    (conde
     [(== '() n) (== '() m)]
     [(== '(1) n) (== '(1) m)]
     [(fresh (n-head n-tail m-head m-tail)
	     (== `(,n-head . ,n-tail) n) (poso n-tail)
	     (== `(,m-head . ,m-tail) m) (poso m-tail)
	     (=lo n-tail m-tail))])))

;; ========= Panel 34 - 45 =========
(define <lo
  (lambda (n m)
    (conde
     [(== '() n) (poso m)]
     [(== '(1) n) (>1o m)]
     [(fresh (n-head n-tail m-head m-tail)
	     (== `(,n-head . ,n-tail) n) (poso n-tail)
	     (== `(,m-head . ,m-tail) n) (poso m-tail)
	     (<lo n-tail m-tail))])))

(define <=lo
  (lambda (n m)
    (condi
     [(=lo n m) succeed]
     [(<lo n m) succeed]
     [fail])))
	     
;; ========= Panel 46 - 52 =========
(define <o
  (lambda (n m)
    (condi
     [(<lo n m) succeed]
     [(=lo n m) (fresh (x)
		       (poso x)
		       (+o n x m))]
     [fail])))

(define <=o
  (lambda (n m)
    (condi
     [(== n m) succeed]
     [(<o n m) succeed]
     [fail])))

;; ========= Panel 53 - END =========
;; I am so sorry, but I really don't want to type that much
