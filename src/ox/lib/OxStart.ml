open Printf
open Packet
open OpenFlow0x01_Core
open OpenFlow0x01
open OxShared

module type OXMODULE = sig
  val switch_connected : switchId -> OpenFlow0x01.SwitchFeatures.t -> unit
  val switch_disconnected : switchId -> unit
  val packet_in : switchId -> xid -> PacketIn.t -> unit
  val barrier_reply : switchId -> xid -> unit
  val stats_reply : switchId -> xid -> StatsReply.t -> unit
  val cleanup : unit -> unit
end

module DefaultTutorialHandlers = struct

  (* I've intentionally omitted packet_in from this structure. *)

  let switch_disconnected (sw : switchId) : unit = ()

  let switch_connected (sw : switchId) (feats : SwitchFeatures.t) : unit = ()

  let barrier_reply _ _ = ()

  let stats_reply _ _ _ = ()

  let cleanup _ = ()

end


module Make (Handlers:OXMODULE) = struct

  let rec switch_thread_loop sw = 
    let open Message in
        begin
          match_lwt (Platform.recv_from_switch sw) with
            | (xid, PacketInMsg pktIn) -> Lwt.wrap3 Handlers.packet_in sw xid pktIn
	          | (xid, BarrierReply) -> Lwt.wrap2 Handlers.barrier_reply sw xid
            | (xid, StatsReplyMsg rep) -> Lwt.wrap3 Handlers.stats_reply sw xid rep
            | (xid, msg) -> Log.info_f "ignored a message from %Ld" sw
                
        end >>
          switch_thread_loop sw

  let switch_thread sw =
    try_lwt
      switch_thread_loop sw
    with 
      | Platform.SwitchDisconnected sw -> 
        Log.info_f "switch %Ld disconnected\n%!" sw
      | exn ->
        Log.info_f ~exn:exn  "unhandled exception raised in handler for \
                           switch %Ld" sw

  (* TODO(arjun): IMO, send_to_switch should *never* fail. *)
  let handle_deferred () = 
    let f (sw, xid, msg) =
      try_lwt
        Platform.send_to_switch sw xid msg
      with exn ->
        Log.info_f ~exn:exn "unhandled exception sending a message to switch \
                             %Ld"  sw 
    in
    Lwt_stream.iter_s f to_send_stream

  let rec accept_switches () = 
    let open Message in
    let open FlowMod in
    lwt feats = Platform.accept_switch () in 
    let sw = feats.SwitchFeatures.switch_id in 
    lwt _ = Log.info_f "switch %Ld connected" sw in
    lwt _ = Platform.send_to_switch sw 0l (FlowModMsg delete_all_flows) in
    lwt _ = Platform.send_to_switch sw 1l BarrierRequest in
    (* JNF: wait for barrier reply? *)
    let _ = Handlers.switch_connected sw feats in 
    Lwt.async (fun () -> switch_thread sw);
    accept_switches ()

  let start_controller () : unix Lwt.t = 
    Platform.init_with_port 6633 >>
    Lwt.pick [ handle_deferred (); accept_switches () ]

  let _ =
    (* intentionally on stdout *)
    Format.printf "Ox controller launching...\n%!";
    Sys.catch_break true;
    try
      Lwt_main.run (start_controller ())
    with exn ->
      Handlers.cleanup ();
      Format.printf "Unexpected exception: %s\n%s\n%!"
        (Printexc.to_string exn)
        (Printexc.get_backtrace ());
      exit 1
end
