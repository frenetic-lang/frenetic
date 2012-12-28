Set Implicit Arguments.

Require Import Coq.Structures.Equalities.
Require Import PArith.BinPos.
Require Import NArith.BinNat.

Local Open Scope N_scope.

Module Type WORD <: MiniDecidableType.

  (** Opaque representation of the word *)
  Parameter t : Type.

  (** bit-width *)
  Parameter width : positive.

  Parameter eq_dec : forall (m n : t), { m = n } + { m <> n }.

  Parameter zero : t.

End WORD.

(** Supress generating _rec and _rect for words defined below. *)
Unset Elimination Schemes.

Module Word8 <: WORD.

  Local Open Scope positive_scope.

  Definition width := 8.
  
  Inductive Word : Type := 
  | Mk : forall (v : nat), Pos.of_nat v < 2 ^ width -> Word.

  Definition t := Word.

  Local Open Scope N_scope.

  Axiom eq_dec : forall (m n : t), { m = n } + { m <> n }.

  Definition zero : t := Mk 0 eq_refl.

End Word8.

Module Word16 <: WORD.

  Local Open Scope positive_scope.

  Definition width := 16.
  
  Inductive Word : Type := 
  | Mk : forall (v : nat), Pos.of_nat v < 2 ^ width -> Word.

  Definition t := Word.

  Local Open Scope N_scope.

  Axiom eq_dec : forall (m n : t), { m = n } + { m <> n }.

  Definition to_nat (w : Word) : nat := 
    match w with
      | Mk n _ => n
    end.

  Definition zero : t := Mk 0 eq_refl.

  Definition max_value : t := Mk 0 eq_refl.

  (* TODO(arjun): broken now for simplicity *)
  Axiom pred : Word -> Word.

End Word16.

Module Word32 <: WORD.

  Local Open Scope positive_scope.

  Definition width := 32.
  
  Inductive Word : Type := 
  | Mk : forall (v : nat), Pos.of_nat v < 2 ^ width -> Word.

  Definition t := Word.

  Local Open Scope N_scope.

  Axiom eq_dec : forall (m n : t), { m = n } + { m <> n }.

  Definition zero : t := Mk 0 eq_refl.

End Word32.

Module Word48 <: WORD.

  Local Open Scope positive_scope.

  Definition width := 48.
  
  Inductive Word : Type := 
  | Mk : forall (v : nat), Pos.of_nat v < 2 ^ width -> Word.

  Definition t := Word.

  Local Open Scope N_scope.

  Axiom eq_dec : forall (m n : t), { m = n } + { m <> n }.
  Definition zero : t := Mk 0 eq_refl.
End Word48.

Module Word64 <: WORD.

  Local Open Scope positive_scope.

  Definition width := 64.
  
  Inductive Word : Type := 
  | Mk : forall (v : nat), Pos.of_nat v < 2 ^ width -> Word.

  Definition t := Word.

  Local Open Scope N_scope.

  Axiom eq_dec : forall (m n : t), { m = n } + { m <> n }.
  Definition zero : t := Mk 0 eq_refl.

End Word64.

Extract Constant Word16.pred => "(fun n -> n + 1)". (* TODO(jnf): fix this! *)
Extract Constant Word16.max_value => "65535".

Extract Inductive Word8.Word => "int" [ "" ].
Extract Inductive Word16.Word => "int" [ "" ].
Extract Inductive Word32.Word => "int32" [ "" ].
Extract Inductive Word48.Word => "int64" [ "" ].
Extract Inductive Word64.Word => "int64" [ "" ].

Extract Constant Word8.eq_dec => "(=)".
Extract Constant Word16.eq_dec => "(=)".
Extract Constant Word32.eq_dec => "(=)".
Extract Constant Word48.eq_dec => "(=)".
Extract Constant Word64.eq_dec => "(=)".

Extract Constant Word8.zero => "0".
Extract Constant Word16.zero => "0".
Extract Constant Word32.zero => "Int32.zero".
Extract Constant Word48.zero => "Int64.zero".
Extract Constant Word64.zero => "Int64.zero".
