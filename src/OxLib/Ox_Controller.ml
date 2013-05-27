open Printf
open Packet
open OpenFlow0x01

module Log = Frenetic_Log

let (<&>) = Lwt.(<&>)

module type OXPLATFORM = 
sig
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
end

module Make (Platform:OpenFlow0x01.PLATFORM) (OxModule:OXMODULE) = 
struct
  
  (* global state *)
  let pending = ref []
  let defer thk = pending := thk::!pending 
  let reset () = pending := []
  let go thk = thk ()  
  let process_deferred () = 
    let old_pending = !pending in 
    reset ();
    Lwt_list.iter_s go (List.rev old_pending)

  module OxPlatform = struct      
    let packetOut xid sw pktOut = 
      defer (fun () -> 
	Platform.send_to_switch sw xid (PacketOutMsg pktOut))
	
    let flowMod xid sw flowMod = 
      defer (fun () -> 
	Platform.send_to_switch sw xid (FlowModMsg flowMod))
	
    let statsRequest xid sw req =
      defer (fun () -> 
	Platform.send_to_switch sw xid (StatsRequestMsg req))

    let barrierRequest xid sw = 
      defer (fun () -> 
	Platform.send_to_switch sw xid BarrierRequest)

    let callback n thk = 
      Lwt.async (fun () -> 
	Lwt_unix.sleep n >>
	Lwt.wrap thk)
  end

  module Handlers = OxModule(OxPlatform) 

  let rec switch_thread sw = 
    try_lwt
      lwt _ = process_deferred () in 
      lwt msg = Platform.recv_from_switch sw in 
      match msg with 
        | (xid,PacketInMsg pktIn) -> 
          Handlers.packetIn xid sw pktIn;
          switch_thread sw 
	| (xid,BarrierReply) -> 
	  Handlers.barrierReply xid;
	  switch_thread sw
        | (xid, StatsReplyMsg rep) ->
          Handlers.statsReply xid sw rep;
          switch_thread sw
        | _ -> 
          switch_thread sw 
    with Platform.SwitchDisconnected sw -> 
      Log.printf "Ox_Controller" "switch %Ld disconnected\n%!" sw;
      Handlers.switchDisconnected sw;
      process_deferred ()

  let rec accept_switches () = 
    lwt feats = Platform.accept_switch () in 
    let sw = feats.switch_id in 
    Log.printf "Ox_Controller" "switch %Ld connected\n%!" sw;
    let delete_all = {
      mfModCmd = DeleteFlow;
      mfMatch = Match.all;
      mfPriority = 65535;
      mfActions = [];
      mfCookie = Int64.zero;
      mfIdleTimeOut = Permanent;
      mfHardTimeOut = Permanent;
      mfNotifyWhenRemoved = false;
      mfApplyToPacket = None;
      mfOutPort = None;
      mfCheckOverlap = false } in 
    lwt _ = Platform.send_to_switch sw Int32.zero (FlowModMsg delete_all) in 
    lwt _ = Platform.send_to_switch sw Int32.one BarrierRequest in
    let _ = Handlers.switchConnected sw in 
    accept_switches () <&> switch_thread sw

  let start_controller = accept_switches
end
