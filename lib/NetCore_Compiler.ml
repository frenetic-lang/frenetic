open List
open Packet
open NetCore_Types
open NetCore_Pattern
module OF = OpenFlow0x01_Core

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

  let negate_output out =
    if out = Output.pass then
      Output.drop
    else if out = Output.drop then
      Output.pass
    else
      (* Should never get here. *)
      failwith 
        (Printf.sprintf 
          "Error: CompilePol.negate_output invoked on non-Boolean output (%s)"
          (Output.string_of_action out))

  let rec compile_pred pr sw = 
    match pr with
      | Hdr pat -> 
        [(pat,Output.pass); (all, Output.drop)]
      | OnSwitch sw' ->
        if sw = sw' then
          [(all, Output.pass)] 
        else 
          [(all, Output.drop)]
      | Or (pr1, pr2) ->
        OutputClassifier.union (compile_pred pr1 sw) (compile_pred pr2 sw)
      | And (pr1, pr2) ->
        OutputClassifier.sequence (compile_pred pr1 sw) (compile_pred pr2 sw)
      | Not pr' ->    
        map (fun (a,b) -> (a, negate_output b)) 
          (compile_pred pr' sw @ [(all,Output.drop)])
      | Everything -> 
        [all,Output.pass]
      | Nothing -> 
        [all,Output.drop]

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
      | Filter pred -> compile_pred pred sw
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
        let seq_then_else (pat, b) =
          if b = Output.pass then
            OutputClassifier.sequence [(pat, Output.pass)] then_tbl
          else if b = Output.drop then
            OutputClassifier.sequence [(pat, Output.pass)] else_tbl
          else
            (* Should never get here. *)
            failwith 
              (Printf.sprintf 
                "Error: compile_pred returned a non-Boolean output (%s)"
                (Output.string_of_action b))
          in
        Frenetic_List.concat_map seq_then_else (compile_pred pred sw)

end

module NetCoreCompiler = CompilePol(NetCore_Action.Output)
module NetCoreGroupCompiler = CompilePol(NetCore_Action.Group)
