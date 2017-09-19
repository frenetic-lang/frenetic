open QuickCheck

val arbitrary_dlAddr : Frenetic_Packet.dlAddr arbitrary
val arbitrary_dlTyp  : Frenetic_Packet.dlTyp arbitrary
val arbitrary_dlVlan : (int option *  bool * int) arbitrary
val arbitrary_nwAddr : Frenetic_Packet.nwAddr arbitrary
val arbitrary_nwTos  : Frenetic_Packet.nwTos arbitrary
val arbitrary_nwProto : Frenetic_Packet.nwProto arbitrary
val arbitrary_tpPort : Frenetic_Packet.tpPort arbitrary

val arbitrary_payload : int -> Cstruct.t arbitrary
val arbitrary_arp : Frenetic_Packet.Arp.t arbitrary

val arbitrary_udp : Cstruct.t arbitrary -> Frenetic_Packet.Udp.t arbitrary
val arbitrary_tcp : Cstruct.t arbitrary -> Frenetic_Packet.Tcp.t arbitrary

val arbitrary_ip_unparsable : Frenetic_Packet.Ip.tp arbitrary
val arbitrary_ip : Frenetic_Packet.Ip.tp arbitrary -> Frenetic_Packet.Ip.t arbitrary

val arbitrary_dl_unparsable  : Frenetic_Packet.nw arbitrary
val arbitrary_packet : Frenetic_Packet.nw arbitrary -> Frenetic_Packet.packet arbitrary
