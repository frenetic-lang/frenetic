Set Implicit Arguments.

Require Import Coq.Logic.ProofIrrelevance.
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

Module Type WIDTH.

  Parameter width : positive.

End WIDTH.

Module Type MAKEWORD.

  Local Open Scope N_scope.

  Parameter width : positive.
  
  Inductive Word : Type := 
  | Mk : forall (v : N), v < 2 ^ N.pos width -> Word.

  Definition t := Word.

End MAKEWORD.

Module MakeWord (Width : WIDTH).

  Local Open Scope N_scope.

  Definition width := Width.width.
  
  Inductive Word : Type := 
  | Mk : forall (v : N), v < 2 ^ N.pos width -> Word.

  Definition t := Word.

  Definition zero : t := @Mk 0 eq_refl.

End MakeWord.

Module Width8 <: WIDTH.

  Definition width := 8 %positive.

End Width8.

Module Width12 <: WIDTH.

  Definition width := 12 %positive.

End Width12.

Module Width16 <: WIDTH.

  Definition width := 16 %positive.

End Width16.

Module Width32 <: WIDTH.

  Definition width := 32 %positive.

End Width32.

Module Width48 <: WIDTH.

  Definition width := 48 %positive.

End Width48.

Module Width64 <: WIDTH.

  Definition width := 64 %positive.

End Width64.

(** Semantically, this module is equivalent to:
  
      Module Word8 := MakeWord (Width8).

    However, the more elaborate definition below allows us to extract
    words of different widths to different OCaml types. *)
Module Word8 <: WORD.

  Module M := MakeWord (Width8).
  Include M.
  Parameter eq_dec : forall (m n : t), { m = n } + { m <> n }.

End Word8.

Module Word12 <: WORD.

  Module M := MakeWord (Width12).
  Include M.
  Parameter eq_dec : forall (m n : t), { m = n } + { m <> n }.

End Word12.

Module Word16 <: WORD.

  Module M := MakeWord (Width16).

  Include M.
  Parameter eq_dec : forall (m n : t), { m = n } + { m <> n }.

  Definition to_nat (w : Word) : nat := 
    match w with
      | Mk n _ => N.to_nat n
    end.

  Definition max_value : t := @Mk 65535 eq_refl.

  (* TODO(arjun): broken now for simplicity *)
  Axiom pred : Word -> Word.

End Word16.

Module Word32 <: WORD.

  Module M := MakeWord (Width32).
  Include M.
  Parameter eq_dec : forall (m n : t), { m = n } + { m <> n }.
End Word32.

Module Word48 <: WORD.

  Module M := MakeWord (Width48).
  Include M.
  Parameter eq_dec : forall (m n : t), { m = n } + { m <> n }.

End Word48.

Module Word64 <: WORD.
  
  Module M := MakeWord (Width64).
  Include M.
  Parameter eq_dec : forall (m n : t), { m = n } + { m <> n }.

End Word64.

Extract Constant Word16.pred => "(fun n -> if n = 0 then 0 else n - 1)". (* TODO: really fix this (JNF) *)
Extract Constant Word16.max_value => "65535".

Extract Inductive Word8.Word => "int" [ "" ].
Extract Inductive Word12.Word => "int" [ "" ].
Extract Inductive Word16.Word => "int" [ "" ].
Extract Inductive Word32.Word => "int32" [ "" ].
Extract Inductive Word48.Word => "int64" [ "" ].
Extract Inductive Word64.Word => "int64" [ "" ].

Extract Constant Word8.eq_dec => "(=)".
Extract Constant Word12.eq_dec => "(=)".
Extract Constant Word16.eq_dec => "(=)".
Extract Constant Word32.eq_dec => "(=)".
Extract Constant Word48.eq_dec => "(=)".
Extract Constant Word64.eq_dec => "(=)".

Extract Constant Word8.zero => "0".
Extract Constant Word12.zero => "0".
Extract Constant Word16.zero => "0".
Extract Constant Word32.zero => "Int32.zero".
Extract Constant Word48.zero => "Int64.zero".
Extract Constant Word64.zero => "Int64.zero".

Extract Constant Word16.to_nat => "(fun x -> x)".
