open List
open Packet
open NetCore_Types
module OF = OpenFlow0x01_Core

module BoolClassifier = NetCore_Classifier.Make(NetCore_Action.Bool)

module GroupClassifier = NetCore_Classifier.Make(NetCore_Action.Group)

module CompilePol (Output : NetCore_Action.COMPILER_ACTION) = struct

  module OutputClassifier = NetCore_Classifier.Make(Output)

  let rec compile_pred pr sw : BoolClassifier.t = 
    match pr with
      | Hdr pat -> 
	[(pat,true); (all, false)]
      | OnSwitch sw' ->
	if sw = sw' then
	  [(all, true)] 
	else 
	  [(all, false)]
      | Or (pr1, pr2) ->
	BoolClassifier.union (compile_pred pr1 sw) (compile_pred pr2 sw)
      | And (pr1, pr2) ->
	BoolClassifier.sequence (compile_pred pr1 sw) (compile_pred pr2 sw)
      | Not pr' ->    
	map (fun (a,b) -> (a, not b)) 
	  (compile_pred pr' sw @ [(all,false)])
      | Everything -> 
	[all,true]
      | Nothing -> 
	[all,false]

  let rec compile_pol p sw =
    match p with
      | HandleSwitchEvent _ -> [(all, Output.drop)]
      | Action action ->
	fold_right 
	  (fun e0 tbl -> 
            OutputClassifier.union 
              [(Output.domain e0, Output.to_action e0)]
              tbl)
	  (Output.atoms (Output.from_nc_action action))
	  [(all, Output.drop)]
      | ActionChoice _ -> failwith "NYI compile_pol ActionChoice"
      | Filter pred ->
	map 
	  (fun (a,b) -> match b with
	    | true -> (a, Output.pass)
	    | false -> (a, Output.drop))
	  (compile_pred pred sw)
      | Union (pol1, pol2) ->
	OutputClassifier.union 
	  (compile_pol pol1 sw) 
	  (compile_pol pol2 sw)
      | Seq (pol1, pol2) ->
	OutputClassifier.sequence 
	  (compile_pol pol1 sw) 
	  (compile_pol pol2 sw)
      | ITE (pred, then_pol, else_pol) ->
	let then_tbl = compile_pol then_pol sw in
	let else_tbl = compile_pol else_pol sw in
	let seq_then_else (pat, b) = match b with
	  | true ->
            OutputClassifier.sequence
              [(pat, Output.pass)] then_tbl
	  | false ->
            OutputClassifier.sequence
              [(pat, Output.pass)] else_tbl in
	Frenetic_List.concat_map seq_then_else (compile_pred pred sw)

  let to_rule (pattern, action) = 
    match NetCore_Pattern.to_match pattern with
      | Some match_ ->
	Some (match_,
              Output.as_actionSequence 
		match_.OF.inPort
		action)
      | None -> None

  let to_query atom (pattern, action) =
    let query_atoms = Output.queries action in
    if List.exists (Output.atom_is_equal atom) query_atoms then
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
end

module NetCoreCompiler = CompilePol(NetCore_Action.Output)
module NetCoreGroupCompiler = CompilePol(NetCore_Action.Group)
