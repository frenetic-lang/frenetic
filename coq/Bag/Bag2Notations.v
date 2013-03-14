Require Import Bag.Bag2Defs.


(* TODO(arjun): I'm not paying attention to levels and associativity. Fix. *)
Reserved Notation "{| x |}" (at level 70, no associativity).
Reserved Notation "{| |}" (at level 70, no associativity).
Reserved Notation "x <+> y" (at level 69, right associativity).

Notation "x <+> y" := (union x y) : bag_scope.
Notation "{| x |}" := (singleton x) : bag_scope.
Notation "{| |}" := (empty) : bag_scope.
