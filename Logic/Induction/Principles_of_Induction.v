Set Printing Universes.

From Coq Require Import Arith.
From Coq Require Import Init.Nat.
From Coq Require Import EqNat.

Definition predicate := nat -> Prop.

Check predicate.

Definition psi :=
  forall (P : predicate) ,
    P 0 ->
    (forall n , P n -> P (S n)) ->
    forall n , P n.

Definition psi_converse :=
  forall (P : predicate) ,
    (forall (n : nat) , P n) ->
    P 0 /\ (forall (n : nat) , P n -> P (S n)).

Theorem psi_converse_proof : psi_converse.
Proof.
  intros P true_forall.
  split.
  - apply true_forall.
  - intros n _.
    apply true_forall.
Qed.

Theorem psi_proof : psi.
Proof.
  intros P P0 step.
  apply (nat_ind P P0 step).
Qed.

Definition pci :=
  forall (P : predicate) ,
    (forall n , (forall k , k < n -> P k) -> P n) ->
    (forall n , P n).

Definition pci_converse :=
  forall (P : predicate) ,
    (forall n , P n) ->
    (forall n , (forall k , k < n -> P k) -> P n).

Theorem pci_converse_proof : pci_converse.
Proof.
  intros P true_forall.
  intros n _.
  apply true_forall.
Qed.

Theorem pci_proof : pci.
Proof.
  intros P.
  intros forall_steppable.
  intros n.
  induction n as [ | n'].
  {
    apply forall_steppable.
    intros k k_lt_zero.
    Search (_ < 0).
    exfalso.
    apply (Nat.nlt_0_r k k_lt_zero).
  }
  {
    admit.
  }
Admitted.
