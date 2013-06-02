open Printf
open Packet
open OpenFlow0x01

module Log = Frenetic_Log
type xid = Message.xid

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

  type to_sw = switchId * xid * Message.t

  let (to_send_stream, defer) : (to_sw Lwt_stream.t * (to_sw option -> unit))
      = Lwt_stream.create ()

  let munge_exns thunk =
    try_lwt
      Lwt.wrap thunk
    with exn ->
      begin
        Log.printf "Ox" "unhandled exception: %s\nRaised by a callback.\n%s%!"
          (Printexc.to_string exn)
          (Printexc.get_backtrace ());
        Lwt.return ()
      end

  module OxPlatform = struct      
    open Message

    let packetOut xid sw pktOut = defer (Some (sw, xid, PacketOutMsg pktOut))
	
    let flowMod xid sw flowMod = defer (Some (sw, xid, FlowModMsg flowMod))
	
    let statsRequest xid sw req = defer (Some (sw, xid, StatsRequestMsg req))

    let barrierRequest xid sw = defer (Some (sw, xid, BarrierRequest))

    (* TODO(arjun): I'm not happy about this. I want an exception to terminate
       the right swich, unless we have exceptions kill the controller. *)
    let callback (n : float) (thk : unit -> unit) : unit = 
      Lwt.async 
        (fun () -> Lwt_unix.sleep n >> munge_exns thk)
  end

  module Handlers = OxModule(OxPlatform) 

  let rec switch_thread_loop sw = 
    let open Message in
    begin
      match_lwt (Platform.recv_from_switch sw) with
        | (xid, PacketInMsg pktIn) -> Lwt.wrap3 Handlers.packetIn xid sw pktIn
	      | (xid, BarrierReply) -> Lwt.wrap1 Handlers.barrierReply xid
        | (xid, StatsReplyMsg rep) -> Lwt.wrap3 Handlers.statsReply xid sw rep
        | (xid, PortStatusMsg ps) -> Lwt.wrap3 Handlers.portStatus xid sw ps
        | (xid, msg) ->
          (Log.printf "Ox" "ignored a message from %Ld" sw;
           Lwt.return ())
           
    end >>
    switch_thread_loop sw

  let switch_thread sw =
    try_lwt
      switch_thread_loop sw
    with 
      | Platform.SwitchDisconnected sw -> 
        begin
          Log.printf "Ox" "switch %Ld disconnected\n%!" sw;
          Lwt.return ()
        end
      | exn ->
        begin
          Log.printf "Ox" "unhandled exception: %s\nRaised in handler for \
                           switch %Ld" (Printexc.to_string exn) sw;
          Lwt.return ()
        end

  (* TODO(arjun): IMO, send_to_switch should *never* fail. *)
  let handle_deferred () = 
    let f (sw, xid, msg) =
      try_lwt
        Platform.send_to_switch sw xid msg
      with exn ->
        begin
          Log.printf "Ox" "unhandled exception: %s sending a message to switch \
                           %Ld.\n%s%!" 
            (Printexc.to_string exn) sw 
            (Printexc.get_backtrace ());
          Lwt.return ()
        end in
    Lwt_stream.iter_s f to_send_stream

  let rec accept_switches () = 
    let open Message in
    let open FlowMod in
    lwt feats = Platform.accept_switch () in 
    let sw = feats.SwitchFeatures.switch_id in 
    Log.printf "Ox_Controller" "switch %Ld connected\n%!" sw;
    lwt _ = Platform.send_to_switch sw 0l delete_all_flows in
    lwt _ = Platform.send_to_switch sw 1l BarrierRequest in
    (* JNF: wait for barrier reply? *)
    let _ = Handlers.switchConnected sw in 
    Lwt.async (fun () -> switch_thread sw);
    accept_switches ()

  let start_controller () = 
    Lwt.pick [ handle_deferred (); accept_switches () ]
end
