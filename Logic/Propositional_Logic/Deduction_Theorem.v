From Coq Require Import Lists.List. Import ListNotations.
From Coq Require Import Init.Nat.

Inductive reflect (P : Prop) : bool -> Prop :=
| ReflectT (H : P) : reflect P true
| ReflectF (H : ~ P) : reflect P false.

Theorem iff_reflect : forall P b ,
    (b = true <-> P) -> reflect P b.
Proof.
  intros P b.
  intros H.
  destruct b as [t | f] eqn:E.
  - apply ReflectT. apply H. reflexivity.
  - apply ReflectF. unfold not. rewrite <- H. intros contra. discriminate contra.
Qed.

Module WFF.
  
  Inductive wff : Type :=
  | var (n : nat)
  | negate (a : wff)
  | implies (a : wff) (b : wff).

  Definition is_implies_bool (a : wff) : bool :=
    match a with
    | implies _ _ => true
    | _ => false
    end.

  Definition is_implies_prop (a : wff) : Prop :=
    exists (a' b' : wff) , a = implies a' b'.

  Theorem deconstruct_implies : forall (a : wff) ,
      is_implies_bool a = true
      <-> is_implies_prop a.
  Proof.
    intros a.
    split.
    { (* -> *)
      intros a_is_implies.
      destruct a as [n | a' | a' b'] eqn:E.
      - simpl in a_is_implies. discriminate a_is_implies. (* Variable *)
      - simpl in a_is_implies. discriminate a_is_implies. (* Negation *)
      - exists a', b'. reflexivity. (* Implication *)
    }
    { (* <- *)
      intros witness.
      destruct witness as [a' [b' P]].
      rewrite -> P. simpl. reflexivity.
    }
  Qed.

  Fixpoint same_wff_bool (a b : wff) : bool :=
    match a, b with
    | var n, var m => n =? m
    | negate a, negate a' => same_wff_bool a a'
    | implies a b, implies a' b' => same_wff_bool a a' && same_wff_bool b b'
    | _, _ => false
    end.

  Definition same_wff_prop (a b : wff) : Prop :=
    a = b.

  Theorem eqb_refl : forall (a : wff) , same_wff_bool a a = true.
  Proof.
    intros a.
    induction a as [ n | a' IH | l IH_l r IH_r].
    - apply PeanoNat.Nat.eqb_refl.
    - simpl. apply IH.
    - simpl. rewrite -> IH_l. rewrite -> IH_r. reflexivity.
  Qed.
      
  Theorem same_wff_bool_iff_prop : forall (a b : wff) ,
      same_wff_bool a b = true <-> same_wff_prop a b.
  Proof.
    intros a b. split.
    { (* -> *)
      generalize dependent b.
      induction a as [ n | a' IH_neg | l_a IH_l r_a IH_r ].
      
      
      intros a_eqb_b.
      induction a as [ n | a' IH_neg | l_a IH_l r_a IH_r ].
      generalize b.
      { destruct b as [m | b' | l_b r_b ] eqn:Eb.
        { simpl in a_eqb_b. Search (_ =? _).
          destruct (PeanoNat.Nat.eqb_spec n m) as [ prf | contra ].
          - rewrite -> prf. reflexivity.
          - discriminate a_eqb_b.
        }
        { simpl in a_eqb_b. discriminate a_eqb_b. }
        { simpl in a_eqb_b. discriminate a_eqb_b. }
      }
      {
        destruct b as [m | b' | l_b r_b ] eqn:Eb.
        { simpl in a_eqb_b. discriminate a_eqb_b. }
        
        { simpl in a_eqb_b. discriminate a_eqb_b. }
        { simpl in a_eqb_b. discriminate a_eqb_b. }
      admit.
    }
    { (* <- *)
      intros a_eq_b. rewrite -> a_eq_b. apply eqb_refl. 
    }
    Admitted.

  Theorem reflect_is_implies : forall (a : wff) ,
      reflect (is_implies_prop a) (is_implies_bool a).
  Proof.
    intros a.
    apply (iff_reflect _ _ (deconstruct_implies a)).
  Qed.

End WFF.

Module Import WFF.

Fixpoint lift (assignment : nat -> bool) (a : wff) : bool :=
  match a with
  | var n => assignment n
  | negate a' => negb (lift assignment a')
  | implies a' b' => (negb (lift assignment a')) || (lift assignment b')
  end.

Definition tautology (a : wff) : Prop :=
  forall assignment : nat -> bool , lift assignment a = true.

Inductive deduce : list wff -> wff -> Prop :=
| in_context : forall (gamma : list wff) (a : wff) , In a gamma -> deduce gamma a
| is_tautology : forall (gamma : list wff) (a : wff) , tautology a -> deduce gamma a
| mp : forall (gamma : list wff) (a b : wff) , deduce gamma a -> deduce gamma b -> is_implies_prop 
                                                     .
