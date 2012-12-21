Extraction Language Ocaml.

Require Import PArith.BinPos.
Require Import NArith.BinNat.

Extract Inductive nat => "int" [ "0" "succ" ]
  "(fun f0 fS n -> if n = 0 then f0 () else fS (n - 1))".

(** WARNING: risk of overflow, which will generate garbage. *)
Extract Inductive positive => "int" 
  [ "(fun n -> 1 + n lsl 1)"
    "(fun n -> n lsl 1)"  
    (* most-significant bit is always 1 for positive integers *)
    "1" ]
  "(fun lsb1 lsb0 msb1 n ->
      if n = 1 then msb1 1
      else if n land 1 = 1 then lsb1 (n lsr 1)
      else lsb0 (n lsr 1))".

(** WARNING: risk of overflow, which will generate garbage. *)
Extract Inductive N => "int"
  [ (* N0  extracts to 0 *) 
    "0" 
    (* Npos : positive -> N extracts to positive, which also extracts to int,
       thus the OCaml code is typable. *)
    "" ].

(* All this is in Datatypes.ml *)

Extract Inductive sumbool => "bool" [ "true" "false" ].

Extract Inductive bool => "bool" [ "true" "false" ].

Extract Inductive option => "option" [ "Some" "None" ].

Extract Inductive unit => "unit" [ "()" ].

Extract Inductive prod => "(*)" [ "(,)" ].

Extract Inductive list => "list" [ "[]" "(::)" ].

(* The generated comparison_rect is a partial function that explodes on
   inputs other than 0, -1, and +1. *)
Extract Inductive comparison => "int" [ "0" "(-1)" "1" ].

Local Open Scope positive_scope.

Example pos5 := 5 %positive.
Example pos3 := 3 %positive.
Example pos15 := Pos.mul 5 3.
