open Printf
open Packet
open OpenFlow0x01

module Log = Frenetic_Log
type xid = Message.xid

let (<&>) = Lwt.(<&>)

module type OXPLATFORM = 
sig
  val packetOut : xid -> switchId -> PacketOut.t -> unit 
  val flowMod : xid -> switchId -> FlowMod.t -> unit 
  val barrierRequest : xid -> switchId -> unit
  val statsRequest : xid -> switchId -> StatsRequest.t -> unit
  val callback : float -> (unit -> unit) -> unit
end

module type OXMODULE = 
functor (OxPlatform:OXPLATFORM) -> 
sig
  val switchConnected : switchId -> unit 
  val switchDisconnected : switchId -> unit
  val packetIn : xid -> switchId -> PacketIn.t -> unit
  val barrierReply : xid -> unit
  val statsReply : xid -> switchId -> StatsReply.t -> unit 
  val portStatus : xid -> switchId -> PortStatus.t -> unit 
end

module Make (Platform:OpenFlow0x01.PLATFORM) (OxModule:OXMODULE) = 
struct
  
  (* global state *)
  let pending : (unit -> unit Lwt.t) list ref = ref []
  let defer thk = pending := thk::!pending 
  let reset () = pending := []
  let go thk = thk ()  
  let process_deferred () = 
    let old_pending = !pending in 
    reset ();
    Lwt_list.iter_s go (List.rev old_pending)

  module OxPlatform = struct      
    open Message

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

    let callback (n:float) (thk:unit -> unit) : unit = 
      Lwt.async (fun () -> 
	Lwt_unix.sleep n >>
        Lwt.wrap thk)
  end

  module Handlers = OxModule(OxPlatform) 

  (* JNF: refactor to make this a tailcall *)
  let rec switch_thread sw = 
    let open Message in
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
        | (xid, PortStatusMsg ps) ->
	  Log.printf "Ox_Controller" "port status\n%!";
          Handlers.portStatus xid sw ps;
          switch_thread sw
        | _ -> 
          switch_thread sw 
    with Platform.SwitchDisconnected sw -> 
      Log.printf "Ox_Controller" "switch %Ld disconnected\n%!" sw;
      Handlers.switchDisconnected sw;
      process_deferred ()

  let rec accept_switches () = 
    let open Message in
    let open FlowMod in
    lwt feats = Platform.accept_switch () in 
    let sw = feats.Features.switch_id in 
    Log.printf "Ox_Controller" "switch %Ld connected\n%!" sw;
    let delete_all = {
      mod_cmd = DeleteFlow;
      match_ = Match.all;
      priority = 65535;
      actions = [];
      cookie = Int64.zero;
      idle_timeout = Permanent;
      hard_timeout = Permanent;
      notify_when_removed = false;
      apply_to_packet = None;
      out_port = None;
      check_overlap = false } in 
    lwt _ = Platform.send_to_switch sw 0l (FlowModMsg delete_all) in 
    lwt _ = Platform.send_to_switch sw 1l BarrierRequest in
    (* JNF: wait for barrier reply? *)
    let _ = Handlers.switchConnected sw in 
    Lwt.pick [accept_switches (); switch_thread sw]

  let start_controller = accept_switches
end
