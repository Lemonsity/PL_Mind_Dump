#lang pie

(claim + (-> Nat Nat Nat))
(define +
  (λ (n m)
    (iter-Nat n
      m
      (λ (i+m) (add1 i+m)))))

(claim +one (-> Nat Nat))
(define +one
  (λ (n) (add1 n)))

(claim mot-n+0=n (-> Nat U))
(define mot-n+0=n
  (λ (n)
    (= Nat (+ n 0) n)))

(claim step-n+0=n (Pi ([n Nat])
                    (-> (mot-n+0=n n) (mot-n+0=n (add1 n)))))
(define step-n+0=n
  (λ (n)
    (λ (n+0=n)
      (cong n+0=n +one))))


(claim n+0=n (Pi ([n Nat])
               (= Nat (+ n 0) n)))
(define n+0=n
  (λ (n)
    (ind-Nat n
      mot-n+0=n
      (same zero)
      step-n+0=n)))

(claim mot-n+Sm=Sn+m (-> Nat U))
(define mot-n+Sm=Sn+m
  (λ (n)
    (Pi ([m Nat])
      (= Nat (+ n (add1 m)) (add1 (+ n m))))))

(claim step-n+Sm=Sn+m (Pi ([n Nat])
                        (-> (mot-n+Sm=Sn+m n)
                            (mot-n+Sm=Sn+m (add1 n)))))
(define step-n+Sm=Sn+m
  (λ (n)
    (λ (forall-m-n+Sm=Sn+m) ;; This is a Pi type
      ;; Recall we want [forall m, Sn+Sm=S(Sn+m)]
      (λ (m)
        (cong (forall-m-n+Sm=Sn+m m) +one)))))

(claim n+Sm=Sn+m (Pi ([n Nat]
                      [m Nat])
                   (= Nat (+ n (add1 m)) (add1 (+ n m)))))
(define n+Sm=Sn+m
  (λ (n m)
    ((ind-Nat n
       mot-n+Sm=Sn+m
       (λ (m) (same (add1 m))) ;; Recall we want [forall m, 0+Sm = S(0+m)] here
       step-n+Sm=Sn+m) m)))

(claim mot-m+n=n+m (-> Nat U))
(define mot-m+n=n+m
  (λ (n)
    (Pi ([m Nat])
      (= Nat (+ m n) (+ n m)))))

(claim step-m+n=n+m (Pi ([n Nat])
                      (-> (mot-m+n=n+m n)
                          (mot-m+n=n+m (add1 n)))))
(define step-m+n=n+m
  (λ (n)
    (λ (forall-m-m+n=n+m)
      ;; Recall we want [forall m, m+Sn=Sn+m]
      (λ (m)
        ;; 1. Add 1 to both sides
        ;; 2. Replace S(m+n) with m+Sn
        (replace
          (symm (n+Sm=Sn+m m n)) ;; Have to change order
          (λ (ph) (= Nat ph (+ (add1 n) m)))
          ;; Here is step 1
          (cong (forall-m-m+n=n+m m) +one))))))

(claim m+n=n+m (Pi ([n Nat]
                    [m Nat])
                 (= Nat (+ m n) (+ n m))))
(define m+n=n+m
  (λ (n m)
    ((ind-Nat n
       mot-m+n=n+m
       n+0=n
       step-m+n=n+m) m)))