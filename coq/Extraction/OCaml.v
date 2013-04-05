Extraction Language Ocaml.

Require Import Coq.Lists.List.
Require Import PArith.BinPos.
Require Import NArith.BinNat.
Require Import Common.Types.

Require Import ExtrOcamlBasic.
Require Import ExtrOcamlString.
Require Import ExtrOcamlNatInt.

Extraction Blacklist String List.

(** Without this hack, these are "extracted as axioms":

      failwith "AXIOM TO BE REALIZED"
   
   IMO, something in the library is poorly designed. The alternative
   is to Set Extraction Opaque. If we do that, we won't get the opacity
   warning in our code, which we can and should fix. *)
Extract Constant destruct_list => 
  "fun _ -> failwith ""destruct_list axiom""".

Extract Constant exists_last => 
  "fun _ -> failwith ""exists_last axiom""".

Extract Constant nth_in_or_default => 
  "fun _ _ _ -> failwith ""nth_in_or_default axiom""".

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

(* The generated comparison_rect is a partial function that explodes on
   inputs other than 0, -1, and +1. *)
Extract Inductive comparison => "int" [ "0" "(-1)" "1" ].

