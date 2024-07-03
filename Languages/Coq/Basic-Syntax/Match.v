(* =================== Match =================== *)

Inductive myBool : Type :=
    | my_true
    | my_false.

Definition to_be_matched : myBool := myTrue.

(* Syntax for match is closer to Haskell then Pie *)
(* The Following will be evaluated to "true" *)
Compute (match to_be_matched with 
            | my_true => true
            | my_false => false
        end).

(* We can also use underscore when we will not use the content *)
(* The following will also be evaluated to "true" *)
Compute (match to_be_matched with
            | my_true => true
            | _ => false
        end).
