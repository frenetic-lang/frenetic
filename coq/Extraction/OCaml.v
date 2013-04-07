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

(* The generated comparison_rect is a partial function that explodes on
   inputs other than 0, -1, and +1. *)
Extract Inductive comparison => "int" [ "0" "(-1)" "1" ].
