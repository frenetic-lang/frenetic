open ControllerInterface
open OpenFlow0x01.Types
open Packet.Types
open Printf
open NetCore_Syntax

module Lwt_channel = Misc.Lwt_channel

module type HANDLERS = sig

  val get_packet_handler : 
    NetCoreEval.id -> switchId -> portId -> packet -> unit

end

module MakeNetCoreMonad
  (Platform : OpenFlow0x01.PLATFORM) 
  (Handlers : HANDLERS) = struct

  type state = NetCoreController.ncstate
      
  type 'x m = state -> ('x * state) Lwt.t

  let bind (m : 'a m) (k : 'a -> 'b m) : 'b m = fun s ->
    Lwt.bind (m s) (fun (a, s') -> k a s')

  let ret (a : 'a) : 'a m = fun (s : state) -> Lwt.return (a,s)

  let get : state m = fun s -> Lwt.return (s,s)

  let put (s : state) : unit m = fun _ -> Lwt.return ((), s)

  let rec forever (m : unit m) = bind m (fun _ -> forever m)

  (** Channel of events for a single-threaded controller. *)
  let events : event Lwt_channel.t = Lwt_channel.create ()

  let send (sw_id : switchId) (xid : xid) (msg : message) = fun (s : state) ->
    Lwt.catch
      (fun () ->
        Lwt.bind (Platform.send_to_switch sw_id xid msg)
          (fun () -> Lwt.return ((), s)))
      (fun exn ->
        match exn with
          | Platform.SwitchDisconnected sw_id' ->
            Lwt.bind (Lwt_channel.send (SwitchDisconnected sw_id') events)
              (fun () -> Lwt.return ((), s))
          | _ -> Lwt.fail exn)

  let recv : event m = fun (s : state) ->
    Lwt.bind (Lwt_channel.recv events) (fun ev -> Lwt.return (ev, s))

  let recv_from_switch_thread sw_id () = 
    Lwt.catch
      (fun () ->
        let rec loop () = 
          Lwt.bind (Platform.recv_from_switch sw_id )
            (fun (xid,msg) ->
              Lwt.bind
                (Lwt_channel.send (SwitchMessage (sw_id, xid, msg)) events)
                (fun () -> loop ())) in
        loop ())
      (fun exn ->
        match exn with
          | Platform.SwitchDisconnected sw_id' ->
            Lwt_channel.send (SwitchDisconnected sw_id') events
          | _ -> Lwt.fail exn)

  let rec accept_switch_thread () = 
    Lwt.bind (Platform.accept_switch ())
      (fun feats -> 
        printf "[NetCore.ml] SwitchConnected event queued\n%!";
        Lwt.bind (Lwt_channel.send (SwitchConnected feats.switch_id) events)
          (fun () ->
            Lwt.async 
              (recv_from_switch_thread feats.switch_id);
            accept_switch_thread ()))

  let handle_get_packet id switchId portId pkt : unit m = fun state ->
    Lwt.return (Handlers.get_packet_handler id switchId portId pkt, state)

  let run (init : state) (action : 'a m) : 'a Lwt.t = 
    (** TODO(arjun): kill threads etc. *)
    Lwt.async accept_switch_thread;
    Lwt.bind (action init) (fun (result, _) -> Lwt.return result)
end

let drop_all_packets = NetCoreEval.PoAtom (NetCoreEval.PrAll, [])

type eventOrPolicy = 
  | Event of ControllerInterface.event
  | Policy of NetCoreEval.pol

module MakeDynamic
  (Platform : OpenFlow0x01.PLATFORM)
  (Handlers : HANDLERS) = struct

  (* The monad is written in OCaml *)
  module NetCoreMonad = MakeNetCoreMonad (Platform) (Handlers)
  (* The controller is written in Coq *)
  module Controller = NetCoreController.Make (NetCoreMonad)

  let start_controller policy_stream =
    let init_state = { 
      NetCoreController.policy = drop_all_packets; 
      NetCoreController.switches = []
    } in
    let policy_stream = Lwt_stream.map (fun v -> Policy v) policy_stream in
    let event_stream = Lwt_stream.map (fun v -> Event v)
      (Lwt_channel.to_stream NetCoreMonad.events) in
    let event_or_policy_stream = Lwt_stream.choose 
      [ event_stream ; policy_stream ] in
    let body = fun state ->
      Lwt.bind (Lwt_stream.next event_or_policy_stream)
        (fun v -> match v with
          | Event ev -> 
            Controller.handle_event ev state
          | Policy pol ->
            Controller.set_policy pol state) in
    let main = NetCoreMonad.forever body in
    NetCoreMonad.run init_state main

end

module Make (Platform : OpenFlow0x01.PLATFORM) = struct

  let get_pkt_handlers : (int, get_packet_handler) Hashtbl.t = 
    Hashtbl.create 200

  module Handlers : HANDLERS = struct
      
    let get_packet_handler queryId switchId portId packet = 
      printf "[NetCore.ml] Got packet from %Ld\n" switchId;
        (Hashtbl.find get_pkt_handlers queryId) switchId portId packet
  end

  module Controller = MakeDynamic (Platform) (Handlers)

  let start_controller (pol : policy Lwt_stream.t) : unit Lwt.t = 
    Controller.start_controller
      (Lwt_stream.map 
         (fun pol -> 
           NetCore_Syntax.desugar_policy pol get_pkt_handlers)
         pol)

end
