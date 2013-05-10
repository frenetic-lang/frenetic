open BoolAction
open Misc
open List
open Misc
open NetCoreEval
open NetworkPacket
open OpenFlow0x01Types
open WordInterface

module NetCoreClassifier = Classifier.Make(NetCoreAction.NetCoreAction)

module BoolAction = BoolAction.Make(NetCoreAction.NetCoreAction.Pattern)

module BoolClassifier = Classifier.Make(BoolAction)

(** val compile_pred : pred -> switchId -> BoolClassifier.t **)

let rec compile_pred pr sw : BoolClassifier.t = 
  match pr with
  | PrHdr pat -> 
    assert false
(*    [(pat,true)] *)
  | PrOnSwitch sw' ->
    if Word64.eq_dec sw sw'
    then ((Obj.magic Pattern.all), true) :: []
    else []
  | PrOr (pr1, pr2) ->
    BoolClassifier.union (compile_pred pr1 sw) (compile_pred pr2 sw)
  | PrAnd (pr1, pr2) ->
    BoolClassifier.sequence (compile_pred pr1 sw) (compile_pred pr2 sw)
  | PrNot pr' ->
    map (Obj.magic ((fun (a,b) -> (a, not b))))
      ((Obj.magic (compile_pred pr' sw)) @ ((Pattern.all, false) :: []))
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
      NetCoreClassifier.union (((NetCoreAction.NetCoreAction.domain e0),
        (e0 :: [])) :: []) tbl) 
      (NetCoreAction.NetCoreAction.atoms action0)
      (((Obj.magic Pattern.all), NetCoreAction.NetCoreAction.drop) :: [])
  | PoFilter pred0 ->
    map (fun (a,b) -> (a, maybe_action NetCoreAction.NetCoreAction.pass b))
      (compile_pred pred0 sw)
  | PoUnion (pol1, pol2) ->
    NetCoreClassifier.union (compile_pol pol1 sw) (compile_pol pol2 sw)
  | PoSeq (pol1, pol2) ->
    NetCoreClassifier.sequence (compile_pol pol1 sw) (compile_pol pol2 sw)

(** val to_rule :
    (Classifier.pattern * Classifier.action) -> (of_match * action list)
    option **)

let to_rule = function
| (pattern0, action0) ->
  (match NetCoreClassifier.Pattern.to_match pattern0 with
   | Some match_ ->
     Some (match_,
       (NetCoreAction.NetCoreAction.as_actionSequence match_.matchInPort
         action0))
   | None -> None)

(** val flow_table_of_policy :
    switchId -> pol -> (of_match * actionSequence) list **)

let flow_table_of_policy sw pol0 =
  filter_map to_rule (compile_pol pol0 sw)

