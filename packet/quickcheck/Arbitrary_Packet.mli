open QuickCheck

val arbitrary_dlAddr : Packet.dlAddr arbitrary
val arbitrary_dlTyp  : Packet.dlTyp arbitrary
val arbitrary_dlVlan : (int option *  bool * int) arbitrary
val arbitrary_nwAddr : Packet.nwAddr arbitrary
val arbitrary_nwTos  : Packet.nwTos arbitrary
val arbitrary_nwProto : Packet.nwProto arbitrary
val arbitrary_tpPort : Packet.tpPort arbitrary

val arbitrary_payload : int -> Packet.bytes arbitrary
val arbitrary_arp : Packet.Arp.t arbitrary

val arbitrary_udp : Packet.bytes arbitrary -> Packet.Udp.t arbitrary
val arbitrary_tcp : Packet.bytes arbitrary -> Packet.Tcp.t arbitrary

val arbitrary_ip_unparsable : Packet.Ip.tp arbitrary
val arbitrary_ip : Packet.Ip.tp arbitrary -> Packet.Ip.t arbitrary

val arbitrary_dl_unparsable  : Packet.nw arbitrary
val arbitrary_packet : Packet.nw arbitrary -> Packet.packet arbitrary
