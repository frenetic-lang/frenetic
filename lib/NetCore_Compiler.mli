
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

module CompilePol : MAKE

(* module CompilePol (Output : NetCore_Action.COMPILER_ACTION) : sig *)
(*   module OutputClassifier : NetCore_Classifier.CLASSIFIER *)
(*     with type action = Output.t *)

(*   val flow_table_of_policy :  *)
(*     OpenFlow0x01.switchId  *)
(*     -> NetCore_Types.pol  *)
(*     -> (OpenFlow0x01.Match.t * OpenFlow0x01.Action.sequence) list *)

(*   val query_fields_of_policy :  *)
(*     NetCore_Types.pol  *)
(*     -> NetCore_Types.action_atom *)
(*     -> OpenFlow0x01.switchId *)
(*     -> OpenFlow0x01.Match.t list *)

(*   val compile_pol :  *)
(*     NetCore_Types.pol  *)
(*     -> OpenFlow0x01.switchId  *)
(*     -> OutputClassifier.t *)
(* end *)


module NetCoreCompiler : sig
  module OutputClassifier : NetCore_Classifier.CLASSIFIER
    with type action = NetCore_Action.Output.t

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


module NetCoreGroupCompiler : sig
  module OutputClassifier : NetCore_Classifier.CLASSIFIER
    with type action = NetCore_Action.Group.t

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
