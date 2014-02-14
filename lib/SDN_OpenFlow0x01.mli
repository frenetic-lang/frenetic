val from_buffer_id : SDN_Types.bufferId -> int32
val to_payload : OpenFlow0x01_Core.payload -> SDN_Types.payload
val from_payload : SDN_Types.payload -> OpenFlow0x01_Core.payload
val to_reason : OpenFlow0x01_Core.packetInReason -> SDN_Types.packetInReason
val to_packetIn : OpenFlow0x01_Core.packetIn -> SDN_Types.pktIn
val from_pattern : SDN_Types.pattern -> OpenFlow0x01_Core.pattern
val from_group : OpenFlow0x01_Core.portId option -> SDN_Types.group -> OpenFlow0x01_Core.action list
val from_timeout : SDN_Types.timeout -> OpenFlow0x01_Core.timeout
val from_flow : int -> SDN_Types.flow -> OpenFlow0x01_Core.flowMod

module Common : HighLevelSwitch_common.S
  with type of_action = OpenFlow0x01_Core.action
   and type of_portId = OpenFlow0x01_Core.portId
