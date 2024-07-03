(* =================== Types =================== *)

(* Defining a new sum type *)
Inductive myType : Type :=
    | inject1
    | inject2.

(* defining a new inductive type *)
(* or similar types in Haskell *)
Inductive myInductiveType : Type := 
    | O
    | S (n: myInductiveType). 
    (* <constructor> (<identifier> : <type>) ...  *)
