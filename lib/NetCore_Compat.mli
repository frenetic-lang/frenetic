module Compat0x01 :
sig
  val to_of_portId : NetCore_Types.portId -> OpenFlow0x01.portId
  val to_nc_portId : OpenFlow0x01.portId -> NetCore_Types.portId

  val to_nc_features : OpenFlow0x01.SwitchFeatures.t -> NetCore_Types.switchFeatures

  val output_to_of : NetCore_Types.portId option -> NetCore_Types.output -> OpenFlow0x01_Core.action list

  val as_actionSequence : NetCore_Types.portId option -> NetCore_Types.action_atom list -> OpenFlow0x01_Core.action list

  val flow_table_of_policy : NetCore_Types.switchId -> NetCore_Types.pol -> (OpenFlow0x01_Core.pattern * OpenFlow0x01_Core.action list) list
end

module Compat0x04 :
sig
  val to_nc_features : OpenFlow0x04.SwitchFeatures.t -> OpenFlow0x04_Core.portDesc list -> NetCore_Types.switchFeatures
  val flow_table_of_policy : NetCore_Types.switchId -> NetCore_Types.pol -> (OpenFlow0x04_Core.oxmMatch * OpenFlow0x04_Core.action list list) list

  val as_actionSequence : NetCore_Types.portId option -> NetCore_Action.Group.t -> OpenFlow0x04_Core.action list list
end
