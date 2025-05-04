From Coq Require Import Arith.
From Coq Require Import Bool.
From Coq Require Import Init.Nat.
From Coq Require Import PeanoNat.
Import Nat.
From Coq Require Import EqNat.

 Set Printing All.

Inductive MyVec (A : Type) : nat -> Type :=
| vnil : MyVec A 0
| vcons : forall (n : nat) , A -> MyVec A n -> MyVec A (S n).

Theorem vec_type_conversion :
  forall A n m , MyVec A (n + m) = MyVec A (m + n).
Proof.
  intros A n m.
  apply f_equal.
  apply add_comm.
Qed.
  
Fixpoint append {A : Type} {n m : nat} (vn : MyVec A n) (vm : MyVec A m) : MyVec A (n + m) :=
  match vn with
  | vnil _ => vm
  | vcons _ n' x vn' => vcons A (n' + m) x (append vn' vm)
  end.

Print append.

Fixpoint append' (A : Type) (n : nat) (m : nat) (vn : MyVec A n) : (MyVec A m) -> MyVec A (n + m) :=
  fun vm =>
    match vn with
    | vnil _ => vm
    | vcons _ n' x vn' => vcons A (n' + m) x (append' A n' m vn' vm)
    end.

Print append'.

Theorem append_diff_length : forall A n m , (MyVec A n) -> (MyVec A m) -> MyVec A (m + n).
Proof.
  intros A n m.
  intros vn vm.
  rewrite Nat.add_comm.
  apply (append vn vm).
Qed.

(* Fixpoint append_lam (A : Type) (n m : nat) : MyVec A n -> MyVec A m -> MyVec A (n + m) := *)
(*   fun vn vm => *)
(*     match vn with *)
(*     | vnil _ => vm *)
(*     | vcons _ n' x vn' => vcons A (n' + m) x (append_lam A n m vn' vm) *)
(*     end. *)

(* Error: *)
(* In environment *)
(* append_lam : forall (A : Type) (n m : nat), MyVec A n -> MyVec A m -> MyVec A (n + m) *)
(* A : Type *)
(* n : nat *)
(* m : nat *)
(* vn : MyVec A n *)
(* vm : MyVec A m *)
(* The term "vm" has type "MyVec A m" while it is expected to have type "MyVec A (?n@{n1:=0} + m)". *)
