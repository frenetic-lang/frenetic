open Misc
open List
open Misc
open NetCoreEval
open NetworkPacket
open OpenFlow0x01Types
open Word

module OutputClassifier = Classifier.Make(Action.Output)

module BoolClassifier = Classifier.Make(Action.Bool)

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

let rec compile_pol p sw =
  match p with
  | PoAction action ->
    fold_right 
      (fun e0 tbl -> OutputClassifier.union [(Action.Output.domain e0, [e0])] tbl)
      (Action.Output.atoms action)
      [(Pattern.all, Action.Output.drop)]
  | PoFilter pred ->
    map 
      (fun (a,b) -> match b with
      | true -> (a, Action.Output.pass)
      | false -> (a, Action.Output.drop))
      (compile_pred pred sw)
  | PoUnion (pol1, pol2) ->
    OutputClassifier.union 
      (compile_pol pol1 sw) 
      (compile_pol pol2 sw)
  | PoSeq (pol1, pol2) ->
    OutputClassifier.sequence 
      (compile_pol pol1 sw) 
      (compile_pol pol2 sw)

let to_rule = function
| (pattern, action) ->
  (match Pattern.to_match pattern with
   | Some match_ ->
     Some (match_,
           (Action.Output.as_actionSequence match_.matchInPort
              action))
   | None -> None)

let flow_table_of_policy sw pol0 =
  filter_map to_rule (compile_pol pol0 sw)

