open List
open Packet
open NetCore_Types
module OF = OpenFlow0x01_Core

module BoolClassifier = NetCore_Classifier.Make(NetCore_Action.Bool)

module GroupClassifier = NetCore_Classifier.Make(NetCore_Action.Group)

module type POLICYCOMPILER = 
sig
  module OutputClassifier : NetCore_Classifier.CLASSIFIER

  (* val flow_table_of_policy :  *)
  (*   OpenFlow0x01.switchId  *)
  (*   -> NetCore_Types.pol  *)
  (*   -> (OpenFlow0x01.Match.t * OpenFlow0x01.Action.sequence) list *)

  (* val query_fields_of_policy :  *)
  (*   NetCore_Types.pol  *)
  (*   -> NetCore_Types.action_atom *)
  (*   -> OpenFlow0x01.switchId *)
  (*   -> OpenFlow0x01.Match.t list *)

  val compile_pol : 
    NetCore_Types.pol 
    -> OpenFlow0x01.switchId 
    -> OutputClassifier.t
end
    
module type MAKE = functor (Output : NetCore_Action.COMPILER_ACTION0x01) ->
  sig include POLICYCOMPILER end
  with type OutputClassifier.action = Output.t

module CompilePol (Output : NetCore_Action.COMPILER_ACTION0x01) = struct

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

end

module NetCoreCompiler = CompilePol(NetCore_Action.Output)
module NetCoreGroupCompiler = CompilePol(NetCore_Action.Group)
