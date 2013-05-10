open BoolAction
open ClassifierImpl
open Datatypes
open List0
open NetCoreEval
open NetworkPacket
open OpenFlow0x01Types
open Types
open WordInterface

module Classifier = Make(NetCoreAction.NetCoreAction)

module BoolAction = BoolAction.Make(NetCoreAction.NetCoreAction.Pattern)

module BoolClassifier = Make(BoolAction)

(** val compile_pred : pred -> switchId -> BoolClassifier.t **)

let rec compile_pred pr sw =
  match pr with
  | PrHdr pat -> ((Obj.magic pat), true) :: []
  | PrOnSwitch sw' ->
    if Word64.eq_dec sw sw'
    then ((Obj.magic Pattern.all), true) :: []
    else []
  | PrOr (pr1, pr2) ->
    BoolClassifier.union (compile_pred pr1 sw) (compile_pred pr2 sw)
  | PrAnd (pr1, pr2) ->
    BoolClassifier.sequence (compile_pred pr1 sw) (compile_pred pr2 sw)
  | PrNot pr' ->
    map (Obj.magic (second negb))
      (app (Obj.magic (compile_pred pr' sw)) ((Pattern.all, false) :: []))
  | PrAll -> ((Obj.magic Pattern.all), true) :: []
  | PrNone -> ((Obj.magic Pattern.all), false) :: []

(** val maybe_action :
    NetCoreAction.NetCoreAction.t -> bool -> NetCoreAction.NetCoreAction.t **)

let maybe_action a = function
| true -> a
| false -> NetCoreAction.NetCoreAction.drop

(** val compile_pol : pol -> switchId -> Classifier.t **)

let rec compile_pol p sw =
  match p with
  | PoAction action0 ->
    fold_right (fun e0 tbl ->
      Classifier.union (((NetCoreAction.NetCoreAction.domain e0),
        (e0 :: [])) :: []) tbl) (((Obj.magic Pattern.all),
      NetCoreAction.NetCoreAction.drop) :: [])
      (NetCoreAction.NetCoreAction.atoms action0)
  | PoFilter pred0 ->
    map (second (maybe_action NetCoreAction.NetCoreAction.pass))
      (compile_pred pred0 sw)
  | PoUnion (pol1, pol2) ->
    Classifier.union (compile_pol pol1 sw) (compile_pol pol2 sw)
  | PoSeq (pol1, pol2) ->
    Classifier.sequence (compile_pol pol1 sw) (compile_pol pol2 sw)

(** val to_rule :
    (Classifier.pattern * Classifier.action) -> (of_match * action list)
    option **)

let to_rule = function
| (pattern0, action0) ->
  (match Classifier.Pattern.to_match pattern0 with
   | Some match_ ->
     Some (match_,
       (NetCoreAction.NetCoreAction.as_actionSequence match_.matchInPort
         action0))
   | None -> None)

(** val flow_table_of_policy :
    switchId -> pol -> (of_match * actionSequence) list **)

let flow_table_of_policy sw pol0 =
  filter_map to_rule (compile_pol pol0 sw)

