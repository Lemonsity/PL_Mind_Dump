Require Import Coq.Lists.List.
Import Coq.Lists.List.ListNotations.

Inductive even : nat -> Prop :=
| even_O : even 0
| even_S : forall n, odd n -> even (S n)
with odd : nat -> Prop :=
| odd_S : forall n, even n -> odd (S n).

Check even_ind.
Check odd_ind.

Inductive value_type : Type :=
| unit : value_type
| func : value_type -> computation_type -> value_type
with computation_type : Type :=
| U : value_type -> computation_type.

Inductive value : Type :=
| var : nat -> value
| abs : value_type -> computation -> value
with computation : Type :=
| ret : value -> computation
| app : value -> value -> computation.

Inductive value_typing : (list value_type) -> value -> value_type -> Prop :=
| t_var : forall ctx i t, nth_error ctx i = Some t -> value_typing ctx (var i) t
| t_abs : forall ctx t1 c t2, computation_typing (t1 :: ctx) c t2 ->
                     value_typing ctx (abs t1 c) (func t1 t2)
with computation_typing : (list value_type) -> computation -> computation_type -> Prop :=
| t_ret : forall ctx v t, value_typing ctx v t -> computation_typing ctx (ret v) (U t)
| t_app : forall ctx v1 v2 t1 t2, value_typing ctx v1 (func t1 t2) -> value_typing ctx v2 t1 -> computation_typing ctx (app v1 v2) t2.

Check value_typing_ind.
Check computation_typing_ind.


(* forall ctx v c . (ctx |-v v : T -> ctx |-v elab(v) : T) /\ (ctx |-c c : C -> ctx |-c elab(c) : C). *)
(* induction on the natural number size(v) + size((c) *)
