open Printf
open Platform
open Packet
open MessagesDef
open ControllerInterface

module type STATE = sig

  type state

end

module type HANDLERS = sig

  val get_packet_handler : 
    NetCoreSemantics.coq_Id -> switchId -> portId -> packet -> unit

end

module EmptyHandlers : HANDLERS = struct

  let get_packet_handler _ _ _ _ = ()

end

module Make 
  (Platform : PLATFORM) 
  (State : STATE)
  (Handlers : HANDLERS) = struct

  type state = State.state
      
  type 'x m = state -> 'x * state

  let bind (m : 'a m) (k : 'a -> 'b m) : 'b m = fun s ->
    let (a, s') = m s in k a s'

  let ret (a : 'a) : 'a m = fun (s : state) -> (a,s)

  let get : state m = fun s -> (s,s)

  let put (s : state) : unit m = fun _ -> ((), s)

  let rec forever (m : unit m) = bind m (fun _ -> forever m)

  let events : event Event.channel = Event.new_channel ()

  let send (sw_id : switchId) (xid : xid) (msg : message) = fun (s : state) ->
    begin
      try 
        Platform.send_to_switch sw_id xid msg        
      with Platform.SwitchDisconnected sw_id' ->
        Event.sync (Event.send events (SwitchDisconnected sw_id'))
    end;
    ((), s)

  let recv : event m = fun (s : state) ->
    (Event.sync (Event.receive events), s)

  let recv_from_switch_thread sw_id = 
    try
      let rec loop () = 
        let (xid, msg) = Platform.recv_from_switch sw_id in
        Event.sync (Event.send events (SwitchMessage (sw_id, xid, msg)));
        loop () in
      loop ()
    with Platform.SwitchDisconnected sw_id' ->
      Event.sync (Event.send events (SwitchDisconnected sw_id'))

  let rec accept_switch_thread () = 
    let feats = Platform.accept_switch () in
    eprintf "[netcore-monad] SwitchConnected event queued.\n%!";
    Event.sync (Event.send events (SwitchConnected feats.switch_id));
    eprintf "[netcore-monad] SwitchConnected event consumed.\n%!";
    let _ = Thread.create recv_from_switch_thread feats.switch_id in
    accept_switch_thread ()

  let handle_get_packet id switchId portId pkt : unit m = fun state ->
    (Handlers.get_packet_handler id switchId portId pkt, state)

  let run (init : state) (action : 'a m) : 'a = 
    (** TODO(arjun): kill threads etc. *)
    let _ = Thread.create accept_switch_thread () in
    let (result, _) = action init in
    result

end

module NetCoreState = struct

  type state = NetCoreController.ncstate

end

module MakeNetCoreController 
  (Platform : PLATFORM)
  (Handlers : HANDLERS) = struct

  (* The monad is written in OCaml *)
  module NetCoreMonad = Make (Platform) (NetCoreState) (Handlers)
  (* The controller is written in Coq *)
  module Controller = NetCoreController.Make (NetCoreMonad)

  let start_controller pol =
    let init_state = { 
      NetCoreController.policy = pol; 
      NetCoreController.switches = []
    } in
    NetCoreMonad.run init_state Controller.main

  (** We'll do this by comingling OCaml and Coq functions in the monad instead
      of simply calling Controller.main *)
  let set_policy _ = failwith "NYI"

end
