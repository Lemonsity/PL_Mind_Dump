Axiom LEM : forall (P : Prop) , P \/ (~ P).

Theorem by_contra :
  forall (P : Prop) , (~ ~ P) -> P.
Proof.
  unfold not.
  intros P not_not_P.
  destruct (LEM P) as [evi_P | evi_not_P] .
  - assumption.
  - exfalso. apply (not_not_P evi_not_P).
Qed.
  
Theorem neg_forall :
  forall (A : Type) (P : A -> Prop) ,
    (~ forall (x : A) , P x) <-> (exists (x : A) , ~ P x).
Proof.
  intros A P.
  split.
  {
    unfold not.
    intros forall_Px_lead_contra.
    apply by_contra.
    unfold not.
    intros counter_ex_lead_contra.

    apply forall_Px_lead_contra.
    intros x.
    apply by_contra.
    unfold not.
    intros counter_ex.
    assert (exist_counter_ex : exists x : A, P x -> False).
    { exists x . assumption. }
    apply (counter_ex_lead_contra exist_counter_ex).
  }
  {
    intros [x Px_lead_contra] forall_Px.
    apply Px_lead_contra.
    apply forall_Px.
  }
Qed.

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
