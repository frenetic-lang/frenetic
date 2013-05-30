open OpenFlow0x01

module type OXPLATFORM = sig
  val packetOut : Message.xid -> switchId -> PacketOut.t -> unit
  val flowMod : Message.xid -> switchId -> FlowMod.t -> unit
  val barrierRequest : Message.xid -> switchId -> unit
  val statsRequest : Message.xid -> switchId -> StatsRequest.t -> unit
  val callback : float -> (unit -> unit) -> unit
end

module type OXMODULE =
  functor (OxPlatform:OXPLATFORM) ->
sig
  val switchConnected : switchId -> unit
  val switchDisconnected : switchId -> unit
  val packetIn : Message.xid -> switchId -> PacketIn.t -> unit
  val barrierReply : Message.xid -> unit
  val statsReply : Message.xid -> switchId -> StatsReply.t -> unit
  val portStatus : Message.xid -> switchId -> PortStatus.t -> unit
end

module Make :
  functor (Platform:PLATFORM) ->
  functor (OxModule:OXMODULE) ->
sig
  val start_controller : unit -> unit Lwt.t
end
