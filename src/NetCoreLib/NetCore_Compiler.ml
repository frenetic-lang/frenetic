open List
open Packet
open NetCore_Types
module OF = OpenFlow0x01_Core

module OutputClassifier = NetCore_Classifier.Make(NetCore_Action.Output)

module BoolClassifier = NetCore_Classifier.Make(NetCore_Action.Bool)

let rec compile_pred pr sw : BoolClassifier.t = 
  match pr with
  | PrHdr pat -> 
   [(pat,true); (all, false)]
  | PrOnSwitch sw' ->
    if sw = sw' then
      [(all, true)] 
    else 
      [(all, false)]
  | PrOr (pr1, pr2) ->
    BoolClassifier.union (compile_pred pr1 sw) (compile_pred pr2 sw)
  | PrAnd (pr1, pr2) ->
    BoolClassifier.sequence (compile_pred pr1 sw) (compile_pred pr2 sw)
  | PrNot pr' ->    
    map (fun (a,b) -> (a, not b)) 
      (compile_pred pr' sw @ [(all,false)])
  | PrAll -> 
    [all,true]
  | PrNone -> 
    [all,false]

let rec compile_pol p sw =
  match p with
  | HandleSwitchEvent _ -> [(all, NetCore_Action.Output.drop)]
  | PoAction action ->
    fold_right 
      (fun e0 tbl -> 
        OutputClassifier.union 
          [(NetCore_Action.Output.domain e0, NetCore_Action.Output.to_action e0)]
          tbl)
      (NetCore_Action.Output.atoms action)
      [(all, NetCore_Action.Output.drop)]
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
  | PoITE (pred, then_pol, else_pol) ->
    let then_tbl = compile_pol then_pol sw in
    let else_tbl = compile_pol else_pol sw in
    let seq_then_else (pat, b) = match b with
      | true ->
        OutputClassifier.sequence
          [(pat, NetCore_Action.Output.pass)] then_tbl
      | false ->
        OutputClassifier.sequence
          [(pat, NetCore_Action.Output.pass)] else_tbl in
    Frenetic_List.concat_map seq_then_else (compile_pred pred sw)

let to_rule (pattern, action) = 
  match NetCore_Pattern.to_match pattern with
    | Some match_ ->
      Some (match_,
            NetCore_Action.Output.as_actionSequence 
              match_.OF.inPort
              action)
    | None -> None

let to_query atom (pattern, action) =
  let query_atoms = NetCore_Action.Output.queries action in
  if List.exists (NetCore_Action.Output.atom_is_equal atom) query_atoms then
    NetCore_Pattern.to_match pattern
  else None

let flow_table_of_policy sw pol0 =
  List.fold_right 
    (fun p acc -> match to_rule p with None -> acc | Some r -> r::acc)
    (compile_pol pol0 sw)
    []

let query_fields_of_policy pol atom sw =
  List.fold_right
    (fun p acc -> match (to_query atom) p with None -> acc | Some r -> r::acc)
    (compile_pol pol sw)
    []
