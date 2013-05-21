val vlan_none : int
val parse_packet : Cstruct.t -> Packet.packet option
val serialize_packet : Packet.packet -> Cstruct.t
