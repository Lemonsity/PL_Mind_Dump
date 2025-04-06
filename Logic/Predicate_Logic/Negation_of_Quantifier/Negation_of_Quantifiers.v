Definition LEM := forall (P : Prop) , P \/ (~ P).

Theorem LEM__by_contra :
  LEM ->
  forall (P : Prop) , (~ ~ P) -> P.
Proof.
  intros LEM.
  unfold not.
  intros P not_not_P.
  destruct (LEM P) as [evi_P | evi_not_P] .
  - assumption.
  - exfalso. apply (not_not_P evi_not_P).
Qed.
  
Theorem neg_forall :
  LEM ->
  forall (A : Type) (P : A -> Prop) ,
    (~ forall (x : A) , P x) <-> (exists (x : A) , ~ P x).
Proof.
  intros LEM.
  assert (by_contra : forall (P : Prop) , (~ ~ P) -> P).
  { apply (LEM__by_contra LEM). }
  
  intros A P.
  split.
  {
    intros not_all_is_true.

    (** The Challenge

        How would someone choose the witness? *)
    
    apply by_contra.
    intros no_counter_example.

    (** Intuition

        After application (I don't know why...),
        we need to show [forall x . P x]

        Because we assumed [no_counter_example],
        for each [x], we can eliminate [~ P x] *)

    apply not_all_is_true.
    intros x.
    destruct (LEM (P x)) as [Px | not_Px].
    - assumption.
    - exfalso.
      apply no_counter_example.
      exists x. apply not_Px.
  }
  {
    intros [x Px_lead_contra] forall_Px.
    apply Px_lead_contra.
    apply forall_Px.
  }
Qed.

(** Negating an [exists] Formula

    It turns out we don't need LEM for this proof.
    In constructive mathematic, negation is represented
    as the ability to produce [False].

    [->]:
        We want to show any [x] satisfying [P x]
        leads to [False].
        This is immediate because such [P x] immediates
        witness [exists x . P x]
    [<-]:
        Want to show [exists x . P x] leads to contradiction.
        Well yes, because such witness can be applied to
        [forall x . P x -> False]

    The process only requires logical implication *)
Theorem neg_exists :
  forall (A : Type) (P : A -> Prop) ,
    (~ exists (x : A) , P x) <-> (forall (x : A) , ~ P x).
Proof.
  intros A P.
  split.
  {
    intros does_not_exists.
    intros x Px.
    assert (exists_ex : exists x : A, P x).
    { exists x . assumption. }
    apply (does_not_exists exists_ex).
  }
  {
    intros all_not_satisfy.
    intros [x Px].
    apply (all_not_satisfy x Px).
  }
Qed.
