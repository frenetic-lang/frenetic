open List
open Packet
open NetCore_Types.Internal

module OutputClassifier = NetCore_Classifier.Make(NetCore_Action.Output)

module BoolClassifier = NetCore_Classifier.Make(NetCore_Action.Bool)

let rec compile_pred pr sw : BoolClassifier.t = 
  match pr with
  | PrHdr pat -> 
   [(pat,true); (NetCore_Pattern.all, false)]
  | PrOnSwitch sw' ->
    if sw = sw' then
      [(NetCore_Pattern.all, true)] 
    else 
      [(NetCore_Pattern.all, false)]
  | PrOr (pr1, pr2) ->
    BoolClassifier.union (compile_pred pr1 sw) (compile_pred pr2 sw)
  | PrAnd (pr1, pr2) ->
    BoolClassifier.sequence (compile_pred pr1 sw) (compile_pred pr2 sw)
  | PrNot pr' ->    
    map (fun (a,b) -> (a, not b)) 
      (compile_pred pr' sw @ [(NetCore_Pattern.all,false)])
  | PrAll -> 
    [NetCore_Pattern.all,true]
  | PrNone -> 
    [NetCore_Pattern.all,false]

let rec compile_pol p sw =
  match p with
  | PoAction action ->
    fold_right 
      (fun e0 tbl -> 
        OutputClassifier.union 
          [(NetCore_Action.Output.domain e0, NetCore_Action.Output.to_action e0)]
          tbl)
      (NetCore_Action.Output.atoms action)
      [(NetCore_Pattern.all, NetCore_Action.Output.drop)]
  | PoFilter pred ->
    map 
      (fun (a,b) -> match b with
      | true -> (a, NetCore_Action.Output.pass)
      | false -> (a, NetCore_Action.Output.drop))
      (compile_pred pred sw)
  | PoUnion (pol1, pol2) ->
    OutputClassifier.union 
      (compile_pol pol1 sw) 
      (compile_pol pol2 sw)
  | PoSeq (pol1, pol2) ->
    OutputClassifier.sequence 
      (compile_pol pol1 sw) 
      (compile_pol pol2 sw)

let to_rule (pattern, action) = 
  match NetCore_Pattern.to_match pattern with
    | Some match_ ->
      Some (match_,
            NetCore_Action.Output.as_actionSequence match_.OpenFlow0x01.Match.inPort
              action)
    | None -> None

let flow_table_of_policy sw pol0 =
  List.fold_right (fun p acc -> match to_rule p with None -> acc | Some r -> r::acc) (compile_pol pol0 sw) []

