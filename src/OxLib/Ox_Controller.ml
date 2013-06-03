open Printf
open Packet
open OpenFlow0x01

module Log = Frenetic_Log

module Platform = OpenFlow0x01_Platform

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

  let send_packet_out sw xid pktOut =
    defer (Some (sw, xid, PacketOutMsg pktOut))
	  
  let send_flow_mod sw xid flowMod =
    defer (Some (sw, xid, FlowModMsg flowMod))
	  
  let send_stats_request sw xid req =
    defer (Some (sw, xid, StatsRequestMsg req))
    
  let send_barrier_request sw xid =
    defer (Some (sw, xid, BarrierRequest))
    
  (* TODO(arjun): I'm not happy about this. I want an exception to terminate
     the right swich, unless we have exceptions kill the controller. *)
  let timeout (n : float) (thk : unit -> unit) : unit = 
    Lwt.async 
      (fun () -> Lwt_unix.sleep n >> munge_exns thk)
end

module type OXMODULE = sig
  val switch_connected : switchId -> unit
  val switch_disconnected : switchId -> unit
  val packet_in : switchId -> xid -> PacketIn.t -> unit
  val barrier_reply : switchId -> xid -> unit
  val stats_reply : switchId -> xid -> StatsReply.t -> unit
  val port_status : switchId -> xid -> PortStatus.t -> unit
end


module Make (Handlers:OXMODULE) = struct

  let rec switch_thread_loop sw = 
    let open Message in
    begin
      match_lwt (Platform.recv_from_switch sw) with
        | (xid, PacketInMsg pktIn) -> Lwt.wrap3 Handlers.packet_in sw xid pktIn
	      | (xid, BarrierReply) -> Lwt.wrap2 Handlers.barrier_reply sw xid
        | (xid, StatsReplyMsg rep) -> Lwt.wrap3 Handlers.stats_reply sw xid rep
        | (xid, PortStatusMsg ps) -> Lwt.wrap3 Handlers.port_status sw xid ps
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
    let _ = Handlers.switch_connected sw in 
    Lwt.async (fun () -> switch_thread sw);
    accept_switches ()

  let start_controller () = 
    Platform.init_with_port 6633 >>
    Lwt.pick [ handle_deferred (); accept_switches () ]

  let _ =
    Log.printf "Ox" "Controller launching...\n%!";
    Sys.catch_break true;
    try
      Lwt_main.run (start_controller ())
    with exn ->
      Log.printf "Ox" "Unexpected exception: %s\n%s\n%!"
        (Printexc.to_string exn)
        (Printexc.get_backtrace ());
      exit 1

end
