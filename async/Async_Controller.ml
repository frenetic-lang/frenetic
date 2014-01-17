open Core.Std
open Async.Std

module Platform = Async_OpenFlow.Highlevel
module SDN = SDN_Types
module NetKAT = NetKAT_Types

module Log = Async_OpenFlow.Log

let max_pending_connections = 64

let _ = Log.set_level `Debug

let _ = Log.set_output 
          [Log.make_colored_filtered_output 
             [("openflow", "socket");
              ("openflow", "platform");
              ("openflow", "serialization")]]

let switch t local_stream feats : unit Deferred.t =
  let sw_id = feats.SDN.switch_id in
  Log.info "setting up switch %d" (VInt.get_int sw_id);
  let next local = Platform.setup_flow_table t sw_id (local sw_id) in
  Pipe.iter local_stream ~f:next

let start ~f ~port ~init_pol ~pols =
  let module Bus = Async_extra.Bus in
  let cur_pol = ref (f init_pol) in
  let bus = Async_extra.Bus.create true in

  Bus.start bus;

  (* Transfer policies to the bus, keeping track of the latest one so that
   * subsequent switch connections can be initialized with it.
   *
   * NB(seliopou): The block on the bus is necessary to ensure that all switches
   * have their policies updated in lockstep.
   * *)
  let _ = Pipe.iter pols ~f:(fun pol ->
    cur_pol := f pol;
    Bus.write bus !cur_pol;
    Bus.flushed bus) in

  Platform.create ~max_pending_connections:max_pending_connections ~port ()
  >>= function t ->
  Pipe.iter (Platform.accept_switches t) ~f:(fun feats ->
    let pols = Pipe.init (fun w ->
      (* NB(seliopou): The without_pushback here is _very_ important. There can
       * be no opportunity for any other writes to happen to the bus between
       * the write of the cur_pol and the current subscribing to the bus. If
       * that were to happen, then different versions of the policy could be
       * installed on the network, which would be very bad.
       *)
      Pipe.write_without_pushback w !cur_pol;
      ignore (Bus.subscribe_exn bus (Pipe.write_without_pushback w));
      Deferred.unit) in
    switch t pols feats)

let start_static ~f ~port ~pol : unit Deferred.t =
  start f port pol (Async.Std.Pipe.of_list [])
