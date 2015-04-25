open Printf
open Frenetic_Packet
open OpenFlow_Header
open OpenFlow0x01
open Message
open OxShared

module type OXMODULE = sig
  val switch_connected : switchId -> OpenFlow0x01.SwitchFeatures.t -> unit
  val switch_disconnected : switchId -> unit
  val packet_in : switchId -> xid -> packetIn -> unit
  val barrier_reply : switchId -> xid -> unit
  val stats_reply : switchId -> xid -> StatsReply.t -> unit
  val cleanup : unit -> unit
end

module DefaultTutorialHandlers = struct

  (* packet_in is intentionally omitted from this structure. *)

  let switch_disconnected (sw : switchId) : unit = ()

  let switch_connected (sw : switchId) (feats : SwitchFeatures.t) : unit = ()

  let barrier_reply _ _ = ()

  let stats_reply _ _ _ = ()

  let cleanup _ = ()

end


module Make (Handlers:OXMODULE) = struct
  open Async.Std
  open Core.Std

  module Controller = Async_OpenFlow0x01_Controller
  module Log = OxShared.Log

  let send_pkt_out ((sw, xid, msg) : to_sw) : unit Deferred.t =
    match Controller.send sw xid msg with 
      | `Ok  -> 
        return ()
      | `Eof -> 
        Log.error ~tags 
          "unhandled exception sending message to switch %Ld" sw;
        return ()

  let handler (e:Controller.event) : unit Deferred.t = 
    let open Message in
    let open FlowMod in
    let open SwitchFeatures in 
    match e with
    | `Connect (sw, feats) ->
      let res1 = Controller.send sw 0l (FlowModMsg delete_all_flows) in 
      let res2 = Controller.send sw 1l BarrierRequest in 
      return 
        (match res1, res2 with 
          | `Ok, `Ok -> 
            let sw = feats.switch_id in
            Handlers.switch_connected sw feats
          | _ -> ())
    | `Message (sw,hdr, msg) ->
      return 
	(match msg with
	  | PacketInMsg pktIn -> Handlers.packet_in sw hdr.xid pktIn
	  | BarrierReply -> Handlers.barrier_reply sw hdr.xid
	  | StatsReplyMsg rep -> Handlers.stats_reply sw hdr.xid rep
	  | msg -> Log.info ~tags "ignored a message from %Ld" sw)
    | `Disconnect sw -> 
      Log.info "switch %Ld disconnected\n%!" sw;
      return ()

  let start_controller () : unit =
    Controller.init 6633; 
    (Deferred.don't_wait_for
       (Monitor.try_with ~name:"controller" (fun () ->
         Deferred.both 
           (Pipe.iter pkt_out send_pkt_out)
           (Pipe.iter Controller.events handler))
         >>= function
           | Ok ((),()) ->
             exit 0
           | Error exn ->
             Log.error ~tags "Unexpected exception: %s\n%!"
               (Exn.to_string exn);
             exit 1))

  let run () : unit =
    let open Core.Std in
    (* intentionally on stdout *)
    Format.printf "Ox controller launching...\n%!";
    Sys.catch_break true;
    ignore (start_controller ());
    Core.Std.never_returns (Async.Std.Scheduler.go ())
end
