Set Implicit Arguments.

Require Import Coq.Classes.Equivalence.

Local Open Scope list_scope.
Local Open Scope signature_scope.
Local Open Scope equiv_scope.

(* TODO(arjun): I'm not paying attention to levels and associativity. Fix. *)
Reserved Notation "{| x |}" (at level 70, no associativity).
Reserved Notation "{| |}" (at level 70, no associativity).
Reserved Notation "x <+> y" (at level 69, right associativity).

Module Type BAG.

  Parameter elt : Type.

  Parameter t : Type.

  Parameter multiplicity : elt -> t -> nat.

  Definition bag_equiv (b b0 : t) :=
    forall elt, multiplicity elt b = multiplicity elt b0.

  Hint Unfold bag_equiv.

  Lemma  bag_equiv_is_Equivalence : Equivalence bag_equiv.
  Proof with auto.
    split.
    unfold Reflexive...
    unfold Symmetric...
    unfold Transitive. autounfold. intros. rewrite -> H...
  Qed.

  Instance Bag_Equivalence : Equivalence bag_equiv.
  Proof. 
    exact bag_equiv_is_Equivalence.
  Qed.

  Parameter empty : t.
   
  Parameter singleton : elt -> t.

  Parameter union : t -> t -> t.

  Parameter empty_spec : forall e, multiplicity e empty = 0.

  Parameter singleton_spec₁_1  : forall e, multiplicity e (singleton e) = 1.

  Parameter singleton_spec_2 : forall e e0,
    e <> e0 ->
    multiplicity e (singleton e0) = 0.

  Parameter union_spec : forall e x1 x2,
    multiplicity e (union x1 x2) = multiplicity e x1 + multiplicity e x2.

  Parameter to_list : t -> list elt.

  Parameter from_list : list elt -> t.

  Parameter to_list_spec : forall e, from_list (to_list e) === e.

End BAG.
