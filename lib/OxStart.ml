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

  module Controller = Async_OpenFlow.OpenFlow0x01.Controller
  module Log = OxShared.Log
  module Stage = Async_OpenFlow.Stage

  (* TODO(arjun): IMO, send_to_switch should *never* fail. *)
  let send_pkt_out ctl ((sw, xid, msg) : to_sw) =
    let open Controller in
    if has_client_id ctl sw then
      begin
	send ctl sw (xid, msg) 
	>>| function
	  | `Sent _ -> 
	    ()
	  | `Drop exn ->
            Log.error ~tags "unhandled exception sending a message to switch \
                               %Ld" sw
      end
    else 
      begin 
        Log.error ~tags "no such client id %Ld" sw;
        return ()
      end

  let handler ctl e =
    let open Message in
    let open FlowMod in
    match e with
    | `Connect (sw, feats) ->
      Controller.send ctl sw (0l, FlowModMsg delete_all_flows) >>= fun _ ->
      Controller.send ctl sw (1l, BarrierRequest) >>= fun _ ->
      let sw = feats.SwitchFeatures.switch_id in
      return (Handlers.switch_connected sw feats)
    | `Message (sw, (xid, msg)) ->
      return 
	(match msg with
	  | PacketInMsg pktIn -> Handlers.packet_in sw xid pktIn
	  | BarrierReply -> Handlers.barrier_reply sw xid
	  | StatsReplyMsg rep -> Handlers.stats_reply sw xid rep
	  | msg -> Log.info ~tags "ignored a message from %Ld" sw)
    | `Disconnect (sw, exn) ->
      Log.info "switch %Ld disconnected\n%!" sw;
      return ()

  let start_controller () : unit =
    Controller.create ~port:6633 () >>> fun ctl ->
      (Deferred.don't_wait_for
         (Monitor.try_with ~name:"controller" (fun () ->
           let d1 = Pipe.iter pkt_out (send_pkt_out ctl) in 
           let d2 = Pipe.iter (Controller.listen ctl) (handler ctl) in 
           d1 >>= fun () -> d2)
           >>= function
             | Ok () ->
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
