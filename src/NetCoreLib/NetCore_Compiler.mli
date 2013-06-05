val flow_table_of_policy : 
  OpenFlow0x01_Core.switchId 
  -> NetCore_Types.pol 
  -> (OpenFlow0x01_Core.Match.t * OpenFlow0x01_Core.Action.sequence) list

val query_fields_of_policy : 
  NetCore_Types.pol 
  -> NetCore_Types.action_atom
  -> OpenFlow0x01_Core.switchId
  -> OpenFlow0x01_Core.Match.t list

module OutputClassifier : NetCore_Classifier.CLASSIFIER
  with type action = NetCore_Action.Output.t

val compile_pol : 
  NetCore_Types.pol 
  -> OpenFlow0x01_Core.switchId 
  -> OutputClassifier.t
