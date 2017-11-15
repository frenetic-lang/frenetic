module INRIASys = Sys

open Core
open Async
open Printf
open Frenetic_kernel.Packet
open Frenetic_kernel.OpenFlow_Header
open Frenetic_kernel.OpenFlow0x01
open Message

let _ = Logging.set_level `Info

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

let (pkt_out : to_sw Pipe.Reader.t), (defer : to_sw option -> unit) =
  let r, w = Pipe.create () in
  r, function
  | None -> ()
  | Some to_sw -> Pipe.write_without_pushback w to_sw

let munge_exns ?(name="munge_exns") thunk =
  let open Core in
  Monitor.try_with ~name (fun () -> return (thunk ()))
  >>> function
  | Ok () -> ()
  | Error exn ->
     Logging.error ~tags "unhandled exception raised by a callback\n%s"
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
    after (Core.Time.Span.of_sec n)
    >>> fun () -> munge_exns thk
end


module Make (Handlers:OXMODULE) = struct

  module Controller = OpenFlow0x01_Plugin.LowLevel

  let handle_pkt_out ((sw, xid, msg) : to_sw) : unit Deferred.t =
    let open OpenFlow0x01_Plugin in
    Controller.send sw xid msg >>= function
      | RpcOk  ->
        return ()
      | RpcEof ->
        Logging.error ~tags
          "unhandled exception sending message to switch %Ld" sw;
        return ()

  let handler (evt:Frenetic_kernel.OpenFlow.event) : unit Deferred.t =
    let open Message in
    let open FlowMod in
    let open SwitchFeatures in
    match evt with
    | SwitchUp (sw, feats) ->
      let res1 = Controller.send sw 0l (FlowModMsg delete_all_flows) in
      let res2 = Controller.send sw 1l BarrierRequest in
        (Deferred.both res1 res2 >>| function
          | RpcOk, RpcOk ->
            let sf = Frenetic_kernel.OpenFlow.{switch_id = sw; switch_ports = feats} in
            Handlers.switch_connected sw (Frenetic_kernel.OpenFlow.To0x01.from_switch_features sf)
          | _ -> ())
    | SwitchDown sw ->
      Logging.info "switch %Ld disconnected\n%!" sw;
      Handlers.switch_disconnected sw;
      return ()
    | PortUp (sw,port) ->
      Logging.info "Port %ld on Switch %Ld connected\n%!" port sw;
      return ()
    | PortDown (sw,port) ->
      Logging.info "Port %ld on Switch %Ld disconnected\n%!" port sw;
      return ()
    | PacketIn (pipe,sw,port,pl,total_len,reason) ->
      let open Frenetic_kernel.OpenFlow.To0x01 in
      let pktIn = {
        input_payload = from_payload pl
        ; total_len = total_len
        ; port = Int32.to_int_exn port
        ; reason = from_packet_in_reason reason
      } in
      return (Handlers.packet_in sw 0l pktIn)
    | PortStats (sw,rep) -> assert false
    | FlowStats (sw,rep) -> assert false
    (*
    | PortStats (sw,rep)
    | FlowStats (sw,rep) ->
      let (_, rep) = message_from_event evt in
      return (Handlers.stats_reply sw 0l rep)
    *)

  let start () : unit =
    (* intentionally on stdout *)
    Format.printf "Ox controller launching...\n%!";
    INRIASys.catch_break true;
    Controller.start 6633 ;
    Deferred.don't_wait_for
      (Monitor.try_with ~name:"controller" (fun () ->
        Deferred.both
          (Pipe.iter pkt_out handle_pkt_out)
          (Pipe.iter Controller.events handler))
        >>= function
          | Ok ((),()) ->
            exit 0
          | Error exn ->
            Logging.error ~tags "Unexpected exception: %s\n%!"
              (Exn.to_string exn);
            exit 1);
    Core.never_returns (Async.Scheduler.go ())
end
