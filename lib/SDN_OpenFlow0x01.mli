open OpenFlow0x01

exception Invalid_port of int32

val to_payload : payload -> SDN_Types.payload
val to_reason : packetInReason -> SDN_Types.packetInReason
val to_packetIn : packetIn -> SDN_Types.pktIn
val to_flowStats : individualStats -> SDN_Types.flowStats

val from_payload : SDN_Types.payload -> payload
val from_packetOut : SDN_Types.pktOut -> packetOut
val from_pattern : SDN_Types.Pattern.t -> pattern
val from_group : portId option -> SDN_Types.group -> action list
val from_timeout : SDN_Types.timeout -> timeout
val from_flow : int -> SDN_Types.flow -> flowMod

module Common : HighLevelSwitch_common.S
  with type of_action = action
   and type of_portId = portId
