Set Implicit Arguments.

Require Import Coq.Lists.List.
Require Import Common.Types.

Require Export Bag.Bag2Defs.
Require Export Bag.Bag2Tactics.
Require Export Bag.Bag2Notations.

Require Bag.Bag2Lemmas.
Module Bag := Bag.Bag2Lemmas.

Arguments to_list _ _ _ : simpl never.

Hint Rewrite 
  Bag.unions_nil
  Bag.unions_cons
  Bag.map_union
  Bag.unions_app
  map_app 
  Bag.union_assoc 
  Bag.from_list_app
  Bag.from_list_cons
  Bag.union_empty_r 
  Bag.union_empty_l
  Bag.from_list_nil_is_empty
  Bag.unions_map_union_comm
  Bag.unions_map_union_comm2
  Bag.unions_map_bag
 : bag.
