open OpenFlow0x01

val switchConnected : switchId -> unit
val switchDisconnected : switchId -> unit
val packetIn : Message.xid -> switchId -> PacketIn.t -> unit
val barrierReply : Message.xid -> unit
val statsReply : Message.xid -> switchId -> StatsReply.t -> unit
val portStatus : Message.xid -> switchId -> PortStatus.t -> unit
