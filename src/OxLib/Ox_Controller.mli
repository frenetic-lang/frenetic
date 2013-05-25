open OpenFlow0x01

type handler = 
    { handleSwitchConnected : switchId -> unit Lwt.t;
      handleSwitchDisconnected : switchId -> unit Lwt.t;        
      handlePacketIn : packetIn -> unit Lwt.t }

module type MAKE  = functor (Platform : PLATFORM) -> 
  sig
    val start_controller : handler -> unit Lwt.t
  end

module Make : MAKE
