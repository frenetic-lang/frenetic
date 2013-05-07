Require Import OpenFlow13.OpenFlowTypes.
Require Import NetCore.NetCoreCompiler.
(* Require Import FwOF.FwOFExtractableController. *)

Require Import Extraction.OCaml.

Extract Inductive nat => int [ "0" "Pervasives.succ" ]
 "(fun fO fS n -> if n=0 then fO () else fS (n-1))".

Cd "../../ocaml/extracted". 
Recursive Extraction Library NetCoreCompiler.
Recursive Extraction Library OpenFlowTypes.
