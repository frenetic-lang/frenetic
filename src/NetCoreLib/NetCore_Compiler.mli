val flow_table_of_policy : OpenFlow0x01.switchId -> NetCore_Types.Internal.pol -> (OpenFlow0x01.Match.t * OpenFlow0x01.Action.sequence) list

val parse_from_chan  : in_channel -> string -> NetCore_Types.External.policy NetCore_Stream.t

val parse_from_lexbuf : Lexing.lexbuf -> string -> NetCore_Types.External.policy NetCore_Stream.t

module OutputClassifier : NetCore_Classifier.CLASSIFIER
  with type action = NetCore_Action.Output.t

val compile_pol : NetCore_Types.Internal.pol -> OpenFlow0x01.switchId -> OutputClassifier.t
