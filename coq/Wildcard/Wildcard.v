Set Implicit Arguments.

Require Import Common.Types.

(** We use Wildcards to build Patterns. When a pattern field is not present,
    we set it to [WildcardExact 0]. An alternative design is to use have a
    [WildcardNotPresent] variant. However, this complicates the definition of
    [ValidPattern]. We have to state that most fields are not 
    [WildcardNotPresent]. *)
Inductive Wildcard (A : Type) : Type :=
| WildcardExact : A -> Wildcard A
| WildcardAll : Wildcard A
| WildcardNone : Wildcard A.

Implicit Arguments WildcardAll [[A]].
Implicit Arguments WildcardNone [[A]].

Module Wildcard.

  Definition inter {A : Type} (eqdec : Eqdec A) (x y : Wildcard A) :=
    match (x, y) with
      | (_, WildcardNone) => WildcardNone
      | (WildcardNone, _) => WildcardNone
      | (WildcardAll, _) => y
      | (_, WildcardAll) => x
      | (WildcardExact m, WildcardExact n) => 
        if eqdec m n then WildcardExact m else WildcardNone
    end.

  Definition is_all {A : Type} (w : Wildcard A) := 
    match w with
      | WildcardAll => true
      | _ => false
    end.

  Definition is_empty {A : Type} (w : Wildcard A) := 
    match w with
      | WildcardNone => true
      | _ => false
    end.

  Definition is_exact {A : Type} (w : Wildcard A) := 
    match w with
      | WildcardExact _ => true
      | _ => false
    end.

  Lemma eq_dec : forall {A : Type} (eqdec : Eqdec A) (x y : Wildcard A),
    { x = y } + { x <> y }.
  Proof with auto.
    decide equality.
  Defined.
 
  Definition to_option (A : Type) (w : Wildcard A)  :=
    match w as w0 return (w0 <> WildcardNone -> option A) with
      | WildcardExact a => fun _ => Some a
      | WildcardAll => fun _  => None
      | WildcardNone => fun not_null => False_rect _ (not_null eq_refl)
    end.

End Wildcard.
