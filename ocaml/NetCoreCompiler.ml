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

let rec compile_pred pr sw : BoolClassifier.t = 
  match pr with
  | PrHdr pat -> 
   [(pat,true)]
  | PrOnSwitch sw' ->
    if Word64.eq_dec sw sw' then
      [(Pattern.all, true)] 
    else 
      [(Pattern.all, false)]
  | PrOr (pr1, pr2) ->
    BoolClassifier.union (compile_pred pr1 sw) (compile_pred pr2 sw)
  | PrAnd (pr1, pr2) ->
    BoolClassifier.sequence (compile_pred pr1 sw) (compile_pred pr2 sw)
  | PrNot pr' ->    
    map (fun (a,b) -> (a, not b)) 
      (compile_pred pr' sw @ [(Pattern.all,false)])
  | PrAll -> 
    [Pattern.all,true]
  | PrNone -> 
    [Pattern.all,false]

let maybe_action a = function
| true -> a
| false -> NetCoreAction.NetCoreAction.drop

let rec compile_pol p sw =
  match p with
  | PoAction action ->
    fold_right 
      (fun e0 tbl -> NetCoreClassifier.union [(NetCoreAction.NetCoreAction.domain e0, [e0])] tbl)
      (NetCoreAction.NetCoreAction.atoms action)
      [(Pattern.all, NetCoreAction.NetCoreAction.drop)]
  | PoFilter pred ->
    map (fun (a,b) -> (a, maybe_action NetCoreAction.NetCoreAction.pass b))
      (compile_pred pred sw)
  | PoUnion (pol1, pol2) ->
    NetCoreClassifier.union 
      (compile_pol pol1 sw) 
      (compile_pol pol2 sw)
  | PoSeq (pol1, pol2) ->
    NetCoreClassifier.sequence 
      (compile_pol pol1 sw) 
      (compile_pol pol2 sw)

let to_rule = function
| (pattern, action) ->
  (match NetCoreClassifier.Pattern.to_match pattern with
   | Some match_ ->
     Some (match_,
           (NetCoreAction.NetCoreAction.as_actionSequence match_.matchInPort
              action))
   | None -> None)

let flow_table_of_policy sw pol0 =
  filter_map to_rule (compile_pol pol0 sw)

