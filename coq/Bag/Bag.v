Set Implicit Arguments.

Require Import Coq.Classes.Equivalence.
Require Import Coq.Lists.List.
Require Import Coq.Setoids.Setoid.
Require Import Common.Types.

Require Export Bag.BagDef.
Require Export Bag.BagTactics.
Require Export Bag.BagNotation.

Require Bag.BagLemmas.
Module Bag := Bag.BagLemmas.


Add Parametric Morphism (A : Type) (E : Eq A) : Union with signature 
  Bag_equiv E ++> Bag_equiv E ++> Bag_equiv E as union_s_m.
  apply BagLemmas.union_m.
Qed.

Hint Rewrite 
  Bag.unions_app
  map_app 
  Bag.union_assoc 
  Bag.FromList_app
  Bag.from_list_cons
  Bag.union_empty_r 
  Bag.union_empty_l
  Bag.FromList_nil_is_Empty
 : bag.
