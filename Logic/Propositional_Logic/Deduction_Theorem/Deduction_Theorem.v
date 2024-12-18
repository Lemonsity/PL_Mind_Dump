From Coq Require Import Lists.List. Import ListNotations.
From Coq Require Import Init.Nat.

Inductive wff : Type :=
| var (n : nat)
| negate (a : wff)
| implies (a : wff) (b : wff).

Definition is_implies_bool (a : wff) : bool :=
  match a with
  | implies _ _ => true
  | _ => false
  end.



Fixpoint lift (assignment : nat -> bool) (a : wff) : bool :=
  match a with
  | var n => assignment n
  | negate a' => negb (lift assignment a')
  | implies a' b' => (negb (lift assignment a')) || (lift assignment b')
  end.

Definition tautology (a : wff) : Prop :=
  forall assignment : nat -> bool , lift assignment a = true.

Theorem tautology_1 : forall (a : wff) , tautology (implies a a).
Proof.
  intros a assignment.
  simpl.
  destruct (lift assignment a) as [ | ].
  - reflexivity.
  - reflexivity.
Qed.

Definition wff_2 (a b : wff) : wff := implies b (implies a b).

Theorem tautology_2 : forall (a b : wff) , tautology (wff_2 a b).
Proof.
  intros a b assignment.
  simpl.
  destruct (lift assignment b) , (lift assignment a).
  - reflexivity.
  - reflexivity.
  - reflexivity.
  - reflexivity.
Qed.

Definition wff_3 (a b c : wff) : wff :=
  implies
    (implies a c)
    (implies
       (implies a (implies c b))
       (implies a b)).

Theorem tautology_3 : forall (a b c : wff) ,
      tautology (wff_3 a b c).
          
Proof.
  intros a b c assignment.
  simpl.
  destruct (lift assignment a) , (lift assignment b) , (lift assignment c).
  - reflexivity.
  - reflexivity.
  - reflexivity.
  - reflexivity.
  - reflexivity.
  - reflexivity.
  - reflexivity.
  - reflexivity.
Qed.


Inductive deduce (gamma : list wff) : wff -> Prop :=
| is_tauto : forall (a : wff) , (tautology a) -> (deduce gamma a)
| in_context : forall (a : wff) , (In a gamma) -> (deduce gamma a)
| mp : forall (a b : wff) , (deduce gamma a) -> (deduce gamma (implies a b)) -> (deduce gamma b).

Theorem deduction_theorem : forall (gamma : list wff) (a b : wff) (tree : deduce (a :: gamma) b) ,
    deduce gamma (implies a b).
Proof.
  intros gamma a b tree.
  induction tree as
    [ b' b_is_tauto
    | b' b_in_context
    | c b' gamma_deduce_a IH1 gamma_deduce_implies IH2 ].
  {
    apply is_tauto.
    {
      intros assignment.
      simpl.
      rewrite -> b_is_tauto.
      rewrite -> Bool.orb_true_r.
      reflexivity.
    }
  }
  {
    destruct b_in_context as [ a_eq_b | b_in_gamma ].
    {
      apply is_tauto.
      rewrite -> a_eq_b.
      apply tautology_1.
    }
    {
      apply (mp gamma b'). (* gamma is somehow implicit here? *)
      {
        apply (in_context _ _ b_in_gamma).
      }
      {
        apply is_tauto.
        apply tautology_2.
      }
    }
  }            
  { 
    apply (mp _ (implies a (implies c b'))).
    - apply IH2.
    - apply (mp _ (implies a c)).
      + apply IH1.
      + apply is_tauto.
        apply (tautology_3 a b' c).
  }
Qed.

(** =======================================================================================
    Below are previous attempt

    I made things much more complicated than necessary
    But I did got practice with proving decidability
    =======================================================================================
 *)

(**

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

Fixpoint eqb (a b : wff) : bool :=
  match a, b with
  | var n, var m => n =? m
  | negate a, negate a' => eqb a a'
  | implies a b, implies a' b' => eqb a a' && eqb b b'
  | _, _ => false
  end.

Theorem eqb_refl : forall (a : wff) , eqb a a = true.
Proof.
  intros a.
  induction a as [ n | a' IH | l IH_l r IH_r].
  - apply PeanoNat.Nat.eqb_refl.
  - simpl. apply IH.
  - simpl. rewrite -> IH_l. rewrite -> IH_r. reflexivity.
Qed.

Theorem same_wff_bool_iff_prop : forall (a b : wff) ,
    eqb a b = true <-> a = b.
Proof.
  intros a b. split.
  { (* -> *)
    generalize dependent b.
    induction a as [ n | a' IH_neg | l_a IH_l r_a IH_r ].
    { intros b.
      destruct b as [m | b' | l_b r_b ] eqn:Eb.
      - simpl.
        rewrite -> (PeanoNat.Nat.eqb_eq n m).
        intros n_eq_m.
        rewrite -> n_eq_m.
        reflexivity.
      - simpl. intros contra. discriminate contra.
      - simpl. intros contra. discriminate contra.
    }
    { intros b.
      destruct b as [m | b' | l_b r_b ] eqn:Eb.
      - simpl. intros contra. discriminate contra.
      - simpl.
        intros a'_eqb_b'.
        rewrite -> (IH_neg b' a'_eqb_b').
        reflexivity.
      - simpl. intros contra. discriminate contra.
    }
    {
      intros b.
      destruct b as [m | b' | l_b r_b ] eqn:Eb.
      - simpl. intros contra. discriminate contra.
      - simpl. intros contra. discriminate contra.
      - simpl.
        intros H.
        assert (both_side_is_true : (eqb l_a l_b = true /\ eqb r_a r_b = true)).
        { rewrite <- Bool.andb_true_iff.
          apply H.
        }
        destruct both_side_is_true as [la_eqb_lb ra_eqb_rb].
        rewrite -> (IH_l _ la_eqb_lb).
        rewrite -> (IH_r _ ra_eqb_rb).
        reflexivity.
    }
  }
  {
    intros a_eq_b.
    rewrite -> a_eq_b.
    apply eqb_refl.
  }
Qed.

Theorem same_wff_bool_iff_prop' : forall (a b : wff) ,
    a = b <-> eqb a b = true.
Proof.
  symmetry.
  apply same_wff_bool_iff_prop.
Qed.

Theorem reflect_is_implies : forall (a : wff) ,
    reflect (is_implies_prop a) (is_implies_bool a).
Proof.
  intros a.
  apply (iff_reflect _ _ (deconstruct_implies a)).
Qed.

Theorem reflect_wff : forall (a b : wff) ,
    reflect (a = b) (eqb a b).
Proof.
  intros a b.
  apply (iff_reflect _ _ (same_wff_bool_iff_prop a b)).
Qed.
  
Inductive deduce' (gamma : list wff) : list wff -> Prop :=
| start' : deduce' gamma []
| is_tauto' : forall (history : list wff) (a : wff) ,
    (deduce' gamma history) -> (tautology a) -> deduce' gamma (a :: history)
| in_context' : forall (history : list wff) (a : wff) ,
    (deduce' gamma history) -> (In a gamma) -> deduce' gamma (a :: history)
| mp' : forall (history history' : list wff) (a b : wff) ,
    (deduce' gamma (a :: history)) -> (deduce' gamma ((implies a b) :: history'))
    -> deduce' gamma (b :: ((implies a b) :: history') ++ (a :: history)).

Lemma deduction_theorem_is_tautology : forall (b : wff) ,
  (tautology b) -> (forall (gamma : list wff) (a : wff) , exists (history : list wff) , deduce' gamma (implies a b :: history)).
Proof.
  intros b b_is_tautology.
  intros gamma a.
  exists [].
  apply (is_tauto' _ _ _ (start' _)).
  intros assignment.
  simpl.
  rewrite -> b_is_tautology.
  rewrite -> Bool.orb_true_r.
  reflexivity.
Qed.

Lemma deduction_theorem_in_context : forall (b : wff) , forall (gamma : list wff) (a : wff) ,
    (In b (a :: gamma)) -> (exists (history : list wff) , deduce' gamma (implies a b :: history)).
Proof.
  intros b gamma a.
  intros b_in_a_gamma.
  destruct (reflect_wff a b) as [ eq | neq ].
  {
    rewrite -> eq.
    exists [].
    apply is_tauto'.
    apply start'.
    apply tautology_1.
  }
  {
    simpl in b_in_a_gamma.
    destruct b_in_a_gamma as [ a_eq_b | b_in_gamma ].
    { destruct (neq a_eq_b).    (* contradiction *) }
    {
      exists [(wff_2 a b) ; b].
      apply (mp' gamma [] []).
      (* [b] is in [gamma] *)
      apply (in_context' _ _ _ (start' _) b_in_gamma).
      (* [b -> (a -> b)] is a tautology *)
      apply (is_tauto' _ _ _ (start' _) (tautology_2 a b)).
    }
  }
Qed.

(** This prove ends up being much too complicated *)

(*     The addition of history requires strong induction on list *)
(*     Much too painful *)
(*  *)

Theorem deduction_theorem' : forall (gamma history : list wff) (a b : wff) ,
    deduce' (a :: gamma) (b :: history) -> (exists (history' : list wff), deduce' gamma ((implies a b) :: history')).
Proof.
  intros gamma history.
  generalize dependent gamma.
  induction history as [ | h history' IH].
  {
    intros gamma a b.
    intros deduction.
    inversion deduction as [
      | history b' sub_deduction b_is_tautology
      | history b' sub_deduction b_in_a_gamma
      | ].
    {
      apply (deduction_theorem_is_tautology b b_is_tautology _ _).
    }
    {
      apply (deduction_theorem_in_context b gamma a b_in_a_gamma).
    }
  }
  {
    intros gamma a b.
    intros deduction.
    inversion deduction as [
      | history b' sub_deduction b_is_tautology
      | history b' sub_deduction b_in_a_gamma
      | history_c history_c_b c b' deduction_c deduction_c_b].
    {
      apply (deduction_theorem_is_tautology b b_is_tautology _ _).
    }
    {
      apply (deduction_theorem_in_context b gamma a b_in_a_gamma).
    }
    {
      assert (deduction_a_c : (exists (history_a_c : list wff) , deduce' gamma ((implies a c) :: history_a_c))).
      {
Admitted.

 *)
