open Classifier
open Datatypes
open List0
open NetCoreEval
open OpenFlow0x01Types
open Types
open WordInterface

type __ = Obj.t

val compile_pred :
  (bool coq_Classifier -> bool coq_Classifier) -> pred -> switchId -> bool
  coq_Classifier

val apply_act : act -> bool -> act

val compile_pol :
  (__ -> __ coq_Classifier -> __ coq_Classifier) -> pol -> switchId -> act
  coq_Classifier

val strip_empty_rules : 'a1 coq_Classifier -> 'a1 coq_Classifier

val no_opt : 'a1 coq_Classifier -> 'a1 coq_Classifier

val compile_no_opt : pol -> switchId -> act coq_Classifier

val compile_opt : pol -> switchId -> act coq_Classifier

