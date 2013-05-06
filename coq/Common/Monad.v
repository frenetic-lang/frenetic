Set Implicit Arguments.

Reserved Notation "x <- M ; K" (at level 60, right associativity).

Module Type MONAD.

  Parameter m : Type -> Type.

  Parameter bind : forall {A B : Type}, m A -> (A -> m B) -> m B.
  (* return is a reserved word and unit is a common type. *)
  Parameter ret : forall {A : Type}, A -> m A.

End MONAD.

Module Maybe <: MONAD.

  Definition m (A : Type) := option A.

  Definition bind {A B : Type} (m : option A) (f : A -> option B) : option B :=
    match m with
      | None => None
      | Some a => f a
    end.

  Definition ret {A : Type} (x : A) := Some x.

End Maybe.
                    