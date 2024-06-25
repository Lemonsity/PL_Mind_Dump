(* Intros and Simpliciation *)

(** Intros
    Use "intros {variables}" to introduce variables into the context
    It is possible to introduce many at a time

    Variables are either "forall" (Pi type), 
    or a Hypothesis (Function input type)
*)
Theorem axiom_of_identity:
    forall (x: nat), x = x -> (forall (y: nat), y = y).
Proof. 
    intros x. (* Introduced one at a time *)
    intros H y. (* Introduced multiple at a time *)
    reflexivity. 
Qed.

Theorem  axiom_of_identity':
    forall (x: nat), x = x -> (forall (y: nat), y = y).
Proof. 
    intros. (* Introduce all hypotheses *)
    reflexivity. 
Qed.


(** Reflexivity / Simplification
    Use "reflexivity" when "the equation clearly holds by evaluation"
    Use "simpl" to see what happens after evaluation
*)
Theorem one_plus_two:
    1 + 2 = 3.
Proof.
    simpl. (* Evaluated *)
    reflexivity. (* Clearly the same, reflexivity *)
Qed.


(** Rewrite
    Given hypothesis "H", goal "G"
    "rewrite -> H" turn left  side of "H" into right side of "H"
    "rewrite <- H" turn right side of "H" into left  side of "H"
    "rewrite H" defaults to "rewrite -> H"
*)
Theorem equal_transitive:
    forall (a b: nat), a = b -> b = a.
Proof.
    intros.
    rewrite -> H. (* Turn "a" in "b = a" to "b" *)
    (* or *) (* rewrite <- H. *) (* Turn "b" in "b = a" to "a" *)
    reflexivity.
Qed.


(** Case 
    Basic syntax:
    destruct <iden> as [ {<arg_iden>} | ...] eqn:<eqn_iden>
                ^             ^          ^           ^
                |             |          |       What to name the new equation
                |             |     Other constructor argument list
                |    List of arg for the fist constructor
            variable to be destructed
*)
