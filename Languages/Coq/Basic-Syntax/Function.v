(* =================== Function =================== *)

(* Functions use the keyword "Definition" *)

(*                          return type *)
(*                parameter/s   |       *)
(*       identifier    |        |       *)
(*            v        v        v       *)
Definition identity (n: nat) : nat :=
    n.


(* =================== Recursive Function =================== *)

(* Recursive Functions use the keyword "Fixpoint" *)
Fixpoint identity' (n: nat) : nat :=
    match n with
    | 0 => 0
    | S m => S (identity' m) (* NOTE: Here we recursively called "identity' " *)
    end.

Compute (identity' 5).
