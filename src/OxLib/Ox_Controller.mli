open OpenFlow0x01

module type OXPLATFORM = sig
  val packetOut : switchId -> packetOut -> unit
  val flowMod : switchId -> flowMod -> unit 
  val statsRequest : switchId -> statsRequest -> unit
end

module type OXMODULE = 
functor (OxPlatform:OXPLATFORM) -> 
sig
  val switchConnected : switchId -> unit 
  val switchDisconnected : switchId -> unit
  val packetIn : switchId -> packetIn -> unit
  val statsReply : switchId -> statsReply -> unit 
end

module Make : 
  functor (Platform:PLATFORM) -> 
    functor (OxModule:OXMODULE) -> 
sig
  val start_controller : unit -> unit Lwt.t
end
