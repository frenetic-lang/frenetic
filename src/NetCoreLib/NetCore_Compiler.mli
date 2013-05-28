val flow_table_of_policy : 
  OpenFlow0x01.switchId 
  -> NetCore_Types.Internal.pol 
  -> (OpenFlow0x01.Match.t * OpenFlow0x01.Action.sequence) list

val query_fields_of_policy : 
  NetCore_Types.Internal.pol 
  -> NetCore_Types.Internal.action_atom
  -> OpenFlow0x01.switchId
  -> OpenFlow0x01.Match.t list

module OutputClassifier : NetCore_Classifier.CLASSIFIER
  with type action = NetCore_Action.Output.t

val compile_pol : 
  NetCore_Types.Internal.pol 
  -> OpenFlow0x01.switchId 
  -> OutputClassifier.t
