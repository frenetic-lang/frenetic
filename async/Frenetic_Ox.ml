module INRIASys = Sys

open Core.Std
open Async.Std
open Printf
open Frenetic_Packet
open Frenetic_OpenFlow_Header
open Frenetic_OpenFlow0x01
open Message

module Log = Frenetic_Log

let _ = Log.set_level `Info

let tags = [("openflow", "controller")]

type to_sw = switchId * xid * Message.t

module type OXMODULE = sig
  val switch_connected : switchId -> SwitchFeatures.t -> unit
  val switch_disconnected : switchId -> unit
  val packet_in : switchId -> xid -> packetIn -> unit
  val barrier_reply : switchId -> xid -> unit
  val stats_reply : switchId -> xid -> StatsReply.t -> unit
  val cleanup : unit -> unit
end

module DefaultHandlers = struct

  (* packet_in is intentionally omitted from this structure. *)

  let switch_disconnected (sw : switchId) : unit = ()

  let switch_connected (sw : switchId) (feats : SwitchFeatures.t) : unit = ()

  let barrier_reply _ _ = ()

  let stats_reply _ _ _ = ()

  let cleanup _ = ()

end

module Make (Handlers:OXMODULE) = struct

  module Controller = Frenetic_OpenFlow0x01_Controller

  let (pkt_out : to_sw Pipe.Reader.t), (defer : to_sw option -> unit) =
    let r, w = Pipe.create () in
    r, function
      | None -> ()
      | Some to_sw -> Pipe.write_without_pushback w to_sw
        
  let munge_exns ?(name="munge_exns") thunk =
    let open Core.Std in
        Monitor.try_with ~name (fun () -> return (thunk ()))
        >>> function
          | Ok () -> ()
          | Error exn ->
            Log.error ~tags "unhandled exception raised by a callback\n%s"
              (Exn.to_string exn)

  module Platform = struct
      
    let send_packet_out sw xid pktOut =
      defer (Some (sw, xid, PacketOutMsg pktOut))
        
    let send_flow_mod sw xid flowMod =
      defer (Some (sw, xid, FlowModMsg flowMod))
        
    let send_stats_request sw xid req =
      defer (Some (sw, xid, StatsRequestMsg req))
        
    let send_barrier_request sw xid =
      defer (Some (sw, xid, BarrierRequest))
        
    let timeout (n : float) (thk : unit -> unit) : unit = 
      after (Core.Std.Time.Span.of_sec n)
      >>> fun () -> munge_exns thk
  end
    
  let handle_pkt_out ((sw, xid, msg) : to_sw) : unit Deferred.t =
    Controller.send sw xid msg >>= function
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
        (Deferred.both res1 res2 >>| function
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

  let start () : unit =
    (* intentionally on stdout *)
    Format.printf "Ox controller launching...\n%!";
    INRIASys.catch_break true;
    Controller.init 6633; 
    Deferred.don't_wait_for
      (Monitor.try_with ~name:"controller" (fun () ->
        Deferred.both 
          (Pipe.iter pkt_out handle_pkt_out)
          (Pipe.iter Controller.events handler))
        >>= function
          | Ok ((),()) ->
            exit 0
          | Error exn ->
            Log.error ~tags "Unexpected exception: %s\n%!"
              (Exn.to_string exn);
            exit 1);
    Core.Std.never_returns (Async.Std.Scheduler.go ())
end
