val parse_packet : Cstruct.t -> Packet.packet option
val serialize_packet : Packet.packet -> Cstruct.t
val string_of_eth : Packet.packet -> string
