Set Implicit Arguments.

Require Common.Types.

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

Definition weak_simulation
  (S T Ob : Type)
  (step_S : step S Ob)
  (step_T : step T Ob)
  (R : relation S T) :=
  forall (s : S) (t : T),
    R s t  ->
    (forall (s' : S) (a : Ob), step_S s (Some a) s' ->
      exists (t' : T), 
        R s' t' /\
        multistep step_T t (a :: nil) t') /\
    (forall (s' : S), step_S s None s' ->
      exists (t' : T), 
        R s' t' /\
        multistep step_T t nil t').

Definition weak_bisimulation
  (S T A : Type) 
  (step_S : step S A) 
  (step_T : step T A)
  (R : relation S T) :=
  weak_simulation step_S step_T R /\
  weak_simulation step_T step_S (inverse_relation R).