Set Implicit Arguments.

Require Import Coq.Lists.List.
Require Import Coq.Classes.Equivalence.
Require Import Coq.Classes.EquivDec.
Require Import Coq.Classes.Morphisms.
Require Import Coq.Setoids.Setoid.
Require Import Common.Types.
Require Import Omega.

Local Open Scope list_scope.
Local Open Scope signature_scope.
Local Open Scope equiv_scope.

Create HintDb bag.

Inductive bag (A : Type) := 
| Empty : bag A
| Singleton : A -> bag A
| Union : bag A -> bag A -> bag A
| FromList : list A -> bag A.

Implicit Arguments Empty [[A]].
Implicit Arguments Singleton [[A]].
Implicit Arguments Union [[A]].
Implicit Arguments FromList [[A]].

Section Definitions.

  Variable A : Type.
  Variable R : relation A.
  Variable E : Equivalence R.
  Variable ED : EqDec A R.

  Fixpoint multiplicity (x : A) (bag : bag A) := 
    match bag with
      | Empty => 0
      | Singleton y => 
        match equiv_dec x y with
          | left _ => 1
          | right _ => 0
        end
      | Union bag1 bag2 => multiplicity x bag1 + multiplicity x bag2
      | FromList lst =>
        List.fold_right 
        (fun y sum =>
          match equiv_dec x y with
            | left _ => S sum
            | right _ => sum
          end) 0 lst
    end.

  Definition Bag_equiv (x y : bag A) := 
    forall (e : A), multiplicity e x = multiplicity e y.
  
  Hint Constructors bag.
  
  Lemma Bag_equiv_is_Equivalence : Equivalence Bag_equiv.
  Proof with auto.
    split.
    unfold Reflexive. unfold Bag_equiv...
    unfold Symmetric. unfold Bag_equiv.
    intros.
    remember (H e)...
    unfold Transitive. unfold Bag_equiv.
    intros.
    remember (H e).
    remember (H0 e).
    rewrite -> e0...
  Qed.
  
  Instance Bag_Equivalence : Equivalence Bag_equiv.
  Proof.
    exact Bag_equiv_is_Equivalence.
  Qed.

  Fixpoint Mem (x : A) (b : bag A) : Prop := 
    match b with
      | Empty => False
      | Singleton y => x === y
      | Union bag1 bag2 => Mem x bag1 \/ Mem x bag2
      | FromList lst => In x lst
    end.

  Implicit Arguments A.

End Definitions.

Arguments multiplicity [A R E ED] x bag.
Arguments Mem [A R E] x b.
