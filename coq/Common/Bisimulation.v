Set Implicit Arguments.

Require Import Common.Types.
Require Import Coq.Lists.List.

Local Open Scope list_scope.

(** [Coq.Relation.Relation_Definitions.relation] has type [A -> A ->
    Prop], which is not what we want. *)
Definition relation (A B : Type) := A -> B -> Prop.

Definition inverse_relation (A B : Type) (R : relation A B) : relation B A :=
  fun (b : B) (a : A) => R a b.

Definition step (A Ob : Type) := A -> option Ob -> A -> Prop.

Inductive multistep (A Ob : Type) (step : step A Ob)
  : A -> list Ob -> A -> Prop :=
  | multistep_nil : forall a, multistep step a nil a
  | multistep_tau : forall a a0 a1 obs,
    step a None a0 ->
    multistep step a0 obs a1 ->
    multistep step a obs a1
  | multistep_obs : forall a a0 a1 ob obs,
    step a (Some ob) a0 ->
    multistep step a0 obs a1 ->
    multistep step a (ob :: obs) a1.

Hint Constructors multistep.

Lemma multistep_app : forall (A Ob : Type)
  (step : step A Ob)
  (s1 s2 s3 : A)
  (obs1 obs2 obs3: list Ob),
  multistep step s1 obs1 s2 ->
  multistep step s2 obs2 s3 ->
  obs3 = obs1 ++ obs2 ->
  multistep step s1 obs3 s3.
Proof with auto.
  intros.
  generalize dependent obs3.
  induction H; intros...
  simpl in H1. subst...
  apply IHmultistep  with (obs3 := obs3) in H0.
  apply multistep_tau with (a0 := a0)...
  trivial.

  simpl in H2.
  rewrite -> H2.
  apply multistep_obs with (a0 := a0)...
Qed.

Definition list_of_option {A : Type} (opt : option A) : list A :=
  match opt with
    | Some a => [a]
    | None => nil
  end.

Definition weak_simulation
  (S T Ob : Type)
  (step_S : step S Ob)
  (step_T : step T Ob)
  (R : relation S T) :=
  forall (s : S) (t : T),
    R s t  ->
    forall (s' : S) (obs : option Ob), 
      step_S s obs s' ->
      exists (t' : T), 
        R s' t' /\
        multistep step_T t (list_of_option obs) t'.

Definition weak_bisimulation
  (S T A : Type) 
  (step_S : step S A) 
  (step_T : step T A)
  (R : relation S T) :=
  weak_simulation step_S step_T R /\
  weak_simulation step_T step_S (inverse_relation R).
