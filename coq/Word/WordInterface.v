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

  Parameter test_bit : forall (n : N), n < Npos width -> t -> bool.
  Parameter set_bit : forall (n : N), n < Npos width -> t -> t.
  Parameter clear_bit : forall (n : N), n < Npos width -> t -> t.

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
  Axiom test_bit : forall (n : N), n < Npos width -> t -> bool.
  Axiom set_bit : forall (n : N), n < Npos width -> t -> t.
  Axiom clear_bit : forall (n : N), n < Npos width -> t -> t.

End Word8.

Module Word16 <: WORD.

  Local Open Scope positive_scope.

  Definition width := 16.
  
  Inductive Word : Type := 
  | Mk : forall (v : nat), Pos.of_nat v < 2 ^ width -> Word.

  Definition t := Word.

  Local Open Scope N_scope.

  Axiom eq_dec : forall (m n : t), { m = n } + { m <> n }.
  Axiom test_bit : forall (n : N), n < Npos width -> t -> bool.
  Axiom set_bit : forall (n : N), n < Npos width -> t -> t.
  Axiom clear_bit : forall (n : N), n < Npos width -> t -> t.

End Word16.

Module Word32 <: WORD.

  Local Open Scope positive_scope.

  Definition width := 32.
  
  Inductive Word : Type := 
  | Mk : forall (v : nat), Pos.of_nat v < 2 ^ width -> Word.

  Definition t := Word.

  Local Open Scope N_scope.

  Axiom eq_dec : forall (m n : t), { m = n } + { m <> n }.
  Axiom test_bit : forall (n : N), n < Npos width -> t -> bool.
  Axiom set_bit : forall (n : N), n < Npos width -> t -> t.
  Axiom clear_bit : forall (n : N), n < Npos width -> t -> t.

End Word32.

(** Easiest way to represent ethernet addresses *)
Module Word48 <: WORD.

  Local Open Scope positive_scope.

  Definition width := 48.
  
  Inductive Word : Type := 
  | Mk : forall (v : nat), Pos.of_nat v < 2 ^ width -> Word.

  Definition t := Word.

  Local Open Scope N_scope.

  Axiom eq_dec : forall (m n : t), { m = n } + { m <> n }.
  Axiom test_bit : forall (n : N), n < Npos width -> t -> bool.
  Axiom set_bit : forall (n : N), n < Npos width -> t -> t.
  Axiom clear_bit : forall (n : N), n < Npos width -> t -> t.

End Word48.

Module Word64 <: WORD.

  Local Open Scope positive_scope.

  Definition width := 64.
  
  Inductive Word : Type := 
  | Mk : forall (v : nat), Pos.of_nat v < 2 ^ width -> Word.

  Definition t := Word.

  Local Open Scope N_scope.

  Axiom eq_dec : forall (m n : t), { m = n } + { m <> n }.
  Axiom test_bit : forall (n : N), n < Npos width -> t -> bool.
  Axiom set_bit : forall (n : N), n < Npos width -> t -> t.
  Axiom clear_bit : forall (n : N), n < Npos width -> t -> t.

End Word64.

Extract Inductive Word8.Word => "Word.Word8.t" [ "" ].
Extract Constant Word8.eq_dec => "Word.Word8.eq_dec".
Extract Constant Word8.test_bit => "Word.Word8.test_bit".
Extract Constant Word8.set_bit => "Word.Word8.set_bit".
Extract Constant Word8.clear_bit => "Word.Word8.clear_bit".

Extract Inductive Word16.Word => "Word.Word16.t" [ "" ].
Extract Constant Word16.eq_dec => "Word.Word16.eq_dec".
Extract Constant Word16.test_bit => "Word.Word16.test_bit".
Extract Constant Word16.set_bit => "Word.Word16.set_bit".
Extract Constant Word16.clear_bit => "Word.Word16.clear_bit".

Extract Inductive Word32.Word => "Word.Word32.t" [ "" ].
Extract Constant Word32.eq_dec => "Word.Word32.eq_dec".
Extract Constant Word32.test_bit => "Word.Word32.test_bit".
Extract Constant Word32.set_bit => "Word.Word32.set_bit".
Extract Constant Word32.clear_bit => "Word.Word32.clear_bit".

Extract Inductive Word48.Word => "Word.Word48.t" [ "" ].
Extract Constant Word48.eq_dec => "Word.Word48.eq_dec".
Extract Constant Word48.test_bit => "Word.Word48.test_bit".
Extract Constant Word48.set_bit => "Word.Word48.set_bit".
Extract Constant Word48.clear_bit => "Word.Word48.clear_bit".

Extract Inductive Word64.Word => "Word.Word64.t" [ "" ].
Extract Constant Word64.eq_dec => "Word.Word64.eq_dec".
Extract Constant Word64.test_bit => "Word.Word64.test_bit".
Extract Constant Word64.set_bit => "Word.Word64.set_bit".
Extract Constant Word64.clear_bit => "Word.Word64.clear_bit".
