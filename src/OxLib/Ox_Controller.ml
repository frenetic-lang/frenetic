open Printf
open Packet
open OpenFlow0x01

module Log = Frenetic_Log

let (<&>) = Lwt.(<&>)

module type OXPLATFORM = 
sig
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

module Make (Platform:OpenFlow0x01.PLATFORM) (OxModule:OXMODULE) = 
struct
  
  (* global state *)
  let xid = ref Int32.zero 
  let pending = ref []
  let defer thk = pending := thk::!pending 
  let process_deferred () = 
    let old_pending = !pending in 
    pending := [];
    Lwt_list.iter_s (fun thk -> thk ()) (List.rev old_pending)

  module OxPlatform = struct      
    let packetOut sw pktOut = 
      defer (fun () -> 
	xid := Int32.succ !xid;
	Platform.send_to_switch sw !xid (PacketOutMsg pktOut))
	
    let flowMod sw flowMod = 
      defer (fun () -> 
	xid := Int32.succ !xid;
	Platform.send_to_switch sw !xid (FlowModMsg flowMod))
	
    let statsRequest sw req =
      defer (fun () -> 
	xid := Int32.succ !xid;
	Platform.send_to_switch sw !xid (StatsRequestMsg req))
  end

  module Handlers = OxModule(OxPlatform) 

  let rec switch_thread sw = 
    try_lwt
      lwt _ = process_deferred () in 
      lwt msg = Platform.recv_from_switch sw in 
      match msg with 
        | (_,PacketInMsg pktIn) -> 
	  Log.printf "Ox_controller" "got packetin\n%!";
          Handlers.packetIn sw pktIn;
          switch_thread sw 
        | (_, StatsReplyMsg rep) ->
          Handlers.statsReply sw rep;
          switch_thread sw
        | _ -> 
          switch_thread sw 
    with Platform.SwitchDisconnected sw -> 
      Handlers.switchDisconnected sw;
      process_deferred ()

  let rec accept_switches () = 
    lwt feats = Platform.accept_switch () in 
    let sw = feats.switch_id in 
    let _ = Log.printf "Ox_Controller" "switch %Ld connected\n%!" sw in 
    let _ = Handlers.switchConnected sw in 
    switch_thread sw <&> accept_switches ()

  let start_controller = accept_switches
end
