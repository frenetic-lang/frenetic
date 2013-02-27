open Wildcard
open Pattern
open ControllerInterface
open MessagesDef
open Packet
open Platform
open NetCoreEval
open Printf

module type HANDLERS = sig

  val get_packet_handler : 
    NetCoreEval.id -> switchId -> portId -> packet -> unit

end

module MakeNetCoreMonad
  (Platform : PLATFORM) 
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
  (Platform : PLATFORM)
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
            printf "[NetCore.ml] new event, calling handler\n%!";
            Controller.handle_event ev state
          | Policy pol ->
            printf "[NetCore.ml] new policy\n%!";
            Controller.set_policy pol state) in
    let main = NetCoreMonad.forever body in
    NetCoreMonad.run init_state main

end

type get_packet_handler = switchId -> portId -> packet -> unit

type predicate =
  | And of predicate * predicate
  | Or of predicate * predicate
  | Not of predicate
  | All
  | NoPackets
  | Switch of switchId
  | InPort of portId
  | DlSrc of Int64.t
  | DlDst of Int64.t
  | SrcIP of Int32.t
  | DstIP of Int32.t
  | TcpSrcPort of int (** 16-bits, implicitly IP *)
  | TcpDstPort of int (** 16-bits, implicitly IP *)

type action =
  | To of int
  | ToAll
  | GetPacket of get_packet_handler

type policy =
  | Pol of predicate * action list
  | Par of policy * policy (** parallel composition *)
  | Restrict of policy * predicate

let rec predicate_to_string pred = match pred with
  | And (p1,p2) -> Printf.sprintf "(And %s %s)" (predicate_to_string p1) (predicate_to_string p2)
  | Or (p1,p2) -> Printf.sprintf "(Or %s %s)" (predicate_to_string p1) (predicate_to_string p2)
  | Not p1 -> Printf.sprintf "(Not %s)" (predicate_to_string p1)
  | NoPackets -> "None"
  | Switch sw -> Printf.sprintf "(Switch %Ld)" sw
  | InPort pt -> Printf.sprintf "(InPort %d)" pt
  | DlSrc add -> Printf.sprintf "(DlSrc %s)" (Util.string_of_mac add)
  | DlDst add -> Printf.sprintf "(DlDst %s)" (Util.string_of_mac add)
  | All -> "All"
  | TcpSrcPort n ->
    Printf.sprintf "(TcpSrcPort %d)" n
  | TcpDstPort n ->
    Printf.sprintf "(TcpDstPort %d)" n
  | SrcIP n ->
    Printf.sprintf "(SrcIP %ld)" n
  | DstIP n ->
    Printf.sprintf "(DstIP %ld)" n
  
let action_to_string act = match act with
  | To pt -> Printf.sprintf "To %d" pt
  | ToAll -> "ToAll"
  | GetPacket _ -> "GetPacket"

let rec policy_to_string pol = match pol with
  | Pol (pred,acts) -> Printf.sprintf "(%s => [%s])" (predicate_to_string pred) (String.concat ";" (List.map action_to_string acts))
  | Par (p1,p2) -> Printf.sprintf "(Union %s %s)" (policy_to_string p1) (policy_to_string p2)
  | Restrict (p1,p2) -> Printf.sprintf "(restrict %s %s)" (policy_to_string p1) (predicate_to_string p2)

module Make (Platform : PLATFORM) = struct

  let next_id : int ref = ref 0

  let get_pkt_handlers : (int, get_packet_handler) Hashtbl.t = 
    Hashtbl.create 200

  module Handlers : HANDLERS = struct
      
    let get_packet_handler queryId switchId portId packet = 
      printf "[NetCore.ml] Got packet from %Ld\n" switchId;
        (Hashtbl.find get_pkt_handlers queryId) switchId portId packet
  end
          
  let desugar_act act = match act with
    | To pt -> Forward (unmodified, PhysicalPort pt)
    | ToAll -> Forward (unmodified, AllPorts)
    | GetPacket handler ->
      let id = !next_id in
      incr next_id;
      Hashtbl.add get_pkt_handlers id handler;
      ActGetPkt id

  let rec desugar_pred pred = match pred with
    | And (p1, p2) -> 
      PrNot (PrOr (PrNot (desugar_pred p1), PrNot (desugar_pred p2)))
    | Or (p1, p2) ->
      PrOr (desugar_pred p1, desugar_pred p2)
    | Not p -> PrNot (desugar_pred p)
    | All -> PrAll
    | NoPackets -> PrNone
    | Switch swId -> PrOnSwitch swId
    | InPort pt -> PrHdr (Pattern.inPort pt)
    | DlSrc n -> PrHdr (Pattern.dlSrc n)
    | DlDst n -> PrHdr (Pattern.dlDst n)
    | SrcIP n -> PrHdr (Pattern.ipSrc n)
    | DstIP n -> PrHdr (Pattern.ipDst n)
    | TcpSrcPort n -> PrHdr (Pattern.tcpSrcPort n)
    | TcpDstPort n -> PrHdr (Pattern.tcpDstPort n)

  let rec desugar_pol1 pol pred = match pol with
    | Pol (pred', acts) -> 
      PoAtom (desugar_pred (And (pred', pred)), List.map desugar_act acts)
    | Par (pol1, pol2) ->
      PoUnion (desugar_pol1 pol1 pred, desugar_pol1 pol2 pred)
    | Restrict (p1, pr1) ->
      desugar_pol1 p1 (And (pred, pr1))

  let rec desugar_pol pol = desugar_pol1 pol All

  module Controller = MakeDynamic (Platform) (Handlers)

  let clear_handlers () : unit = 
    Hashtbl.clear get_pkt_handlers;
    next_id := 0

  let start_controller (pol : policy Lwt_stream.t) : unit Lwt.t = 
    Controller.start_controller
      (Lwt_stream.map 
         (fun pol -> 
            printf "[NetCore.ml] got a new policy%!\n";
            clear_handlers (); 
            desugar_pol pol)
         pol)

end
