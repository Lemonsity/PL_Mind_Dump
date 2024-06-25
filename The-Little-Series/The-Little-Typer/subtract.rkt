#lang pie

; What we want:
; (- n m) = n - m

; This subtracts 1
(claim sub1
  (-> Nat Nat))
(define sub1
  (λ (n)
    (rec-Nat n
      0
      (λ (n-1 prev)
        n-1))))

; For each value of m, subtract 1
(claim -
  (-> Nat Nat Nat))
(define -
  (λ (n)
    (λ (m)
      (rec-Nat m
        n
        (λ (m-1 prev)
          (sub1 prev))))))
