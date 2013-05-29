open OpenFlow0x01

module type OXPLATFORM = sig
  val packetOut : xid -> switchId -> packetOut -> unit
  val flowMod : xid -> switchId -> flowMod -> unit 
  val barrierRequest : xid -> switchId -> unit
  val statsRequest : xid -> switchId -> statsRequest -> unit
  val callback : float -> (unit -> unit) -> unit
end

module type OXMODULE = 
  functor (OxPlatform:OXPLATFORM) -> 
sig
  val switchConnected : switchId -> unit 
  val switchDisconnected : switchId -> unit
  val packetIn : xid -> switchId -> packetIn -> unit
  val barrierReply : xid -> unit
  val statsReply : xid -> switchId -> statsReply -> unit 
  val portStatus : xid -> switchId -> portStatus -> unit 
end

module Make : 
  functor (Platform:PLATFORM) -> 
  functor (OxModule:OXMODULE) -> 
sig
  val start_controller : unit -> unit Lwt.t
end
