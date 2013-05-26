open Printf
open Packet
open Frenetic_Log
open OpenFlow0x01

let (<&>) = Lwt.(<&>)

type handler = 
    { handleSwitchConnected : switchId -> unit Lwt.t;
      handleSwitchDisconnected : switchId -> unit Lwt.t;        
      handlePacketIn : packetIn -> unit Lwt.t }

module type MAKE = functor (Platform : OpenFlow0x01.PLATFORM) -> 
  sig
    val start_controller : handler -> unit Lwt.t
  end

module Make (Platform : OpenFlow0x01.PLATFORM) = struct

  let rec switch_thread sw handlers = 
    try_lwt
      lwt msg = Platform.recv_from_switch sw in 
      match msg with 
        | (_,PacketInMsg pktIn) -> 
          handlers.handlePacketIn pktIn >>
          switch_thread sw handlers
        | _ -> 
          switch_thread sw handlers
    with Platform.SwitchDisconnected sw -> 
      handlers.handleSwitchDisconnected sw 

  let rec accept_switches handlers = 
    lwt feats = Platform.accept_switch () in 
    let sw = feats.switch_id in 
    let _ = Log.printf "Ox_Controller" "switch %Ld connected\n%!" sw in 
    handlers.handleSwitchConnected sw <&>
    switch_thread sw handlers <&> 
    accept_switches handlers

  let start_controller = accept_switches

end
