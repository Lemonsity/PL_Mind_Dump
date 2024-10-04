Inductive FreeTree {X : Type} {param_map : nat -> Type} {arity_map : nat -> Type} : Type :=
| ret (v : X)
| op (index : nat) (p : param_map index) (k : arity_map index -> FreeTree).

Fixpoint lifting (X Y : Type) (param_map : nat -> Type) (arity_map : nat -> Type)
  (f : X -> @FreeTree Y param_map arity_map)
  (tree : @FreeTree X param_map arity_map) :
  @FreeTree Y param_map arity_map :=
  match tree with
  | ret v => f v
  | op i p k => op i p (fun a => lifting X Y param_map arity_map f (k a))
  end.

    
             

  
      
  
