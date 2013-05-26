open OpenFlow0x01

module type OXPLATFORM = sig
  val packetOut : switchId -> packetOut -> unit Lwt.t
  val flowMod : switchId -> flowMod -> unit Lwt.t
end

module type OXHANDLER = sig
  val switchConnected : switchId -> unit Lwt.t
  val switchDisconnected : switchId -> unit Lwt.t
  val packetIn : packetIn -> unit Lwt.t
end

module MakeOxPlatform : 
  functor (Platform:OpenFlow0x01.PLATFORM) -> 
sig
  include OXPLATFORM  
end 

module Make : 
  functor (Platform:PLATFORM) -> 
    functor (Handler:OXHANDLER) -> 
sig
  val start_controller : unit -> unit Lwt.t
end
