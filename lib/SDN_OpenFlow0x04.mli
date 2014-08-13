
val to_payload : OpenFlow0x04_Core.payload -> SDN_Types.payload
val from_payload : SDN_Types.payload -> OpenFlow0x04_Core.payload
val to_reason : OpenFlow0x04_Core.packetInReason -> SDN_Types.packetInReason
val to_packetIn : OpenFlow0x04_Core.packetIn -> SDN_Types.pktIn
val from_packetOut : SDN_Types.pktOut -> OpenFlow0x04_Core.packetOut
val from_pattern : SDN_Types.Pattern.t -> OpenFlow0x04_Core.oxmMatch * SDN_Types.portId option
val from_group : OpenFlow0x04_Core.portId option -> GroupTable0x04.t -> SDN_Types.group -> OpenFlow0x04_Core.action list
val from_timeout : SDN_Types.timeout -> OpenFlow0x04_Core.timeout
val from_flow : GroupTable0x04.t -> int -> SDN_Types.flow -> OpenFlow0x04_Core.flowMod

val fix_vlan_in_flow : SDN_Types.flow -> SDN_Types.flow list
val fix_vlan_in_table : SDN_Types.flowTable -> SDN_Types.flowTable

module Common : HighLevelSwitch_common.S
  with type of_action = OpenFlow0x04_Core.action
   and type of_portId = OpenFlow0x04_Core.portId
