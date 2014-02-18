open Core.Std
open Async.Std

open Topology

module Controller = Async_OpenFlow.OpenFlow0x01.Controller
module SDN = SDN_Types
module NetKAT = NetKAT_Types

module Log = Async_OpenFlow.Log

let max_pending_connections = 64

let _ = Log.set_level `Debug

let _ = Log.set_output
          [Log.make_colored_filtered_output
             [("openflow", "socket");
              ("openflow", "platform");
              ("openflow", "serialization");
              ("openflow", "controller")]]

let tags = [("openflow", "controller")]

let to_event t evt =
  let open NetKAT_Types in
  return (match evt with
    | `Connect (c_id, feats) ->
      [SwitchUp feats.OpenFlow0x01.SwitchFeatures.switch_id]
    | `Disconnect (c_id, switch_id, exn) ->
      [SwitchDown switch_id]
    | `Message (c_id, (xid, msg)) ->
      let open OpenFlow0x01.Message in
      begin match msg with
        | PacketInMsg pi ->
          let open OpenFlow0x01_Core in
          let switch_id = Controller.switch_id_of_client t c_id in
          let port_id = VInt.Int16 pi.port in
          let buf_id, bytes = match pi.input_payload with
            | Buffered(n, bs) -> Some(n), bs
            | NotBuffered(bs) -> None, bs in
          (* XXX(seliopou): What about non-application-related PacketIns? They
           * may happen while initializing a switch.
           *
           * XXX(seliopou): Any packet_outs should be associated with this
           * PacketIn.
           * *)
          [PacketIn("", switch_id, port_id, bytes, pi.total_len, buf_id)]
        | _ ->
          let sw_id = Controller.switch_id_of_client t c_id in
          Log.debug ~tags "Dropped message from %Lu: %s" sw_id (to_string msg);
          []
      end)

let packet_out_to_message (_, bytes, buffer_id, port_id, actions) =
  let open OpenFlow0x01_Core in
  let output_payload = match buffer_id with
    | Some(id) -> Buffered(id, bytes)
    | None -> NotBuffered bytes in
  let port_id = match port_id with
    | Some(vi) -> Some(VInt.get_int vi)
    | None -> None in
  let apply_actions = SDN_OpenFlow0x01.from_group port_id [[actions]] in
  OpenFlow0x01.Message.PacketOutMsg{ output_payload; port_id; apply_actions }

let send t c_id msg =
  Controller.send t c_id msg
  >>| function
    | `Sent _ -> ()
    | `Drop exn -> raise exn

let update_table_for (t : Controller.t) (sw_id : switchId) pol =
  let delete_flows =
    OpenFlow0x01.Message.FlowModMsg OpenFlow0x01_Core.delete_all_flows in
  let to_flow_mod prio flow =
    OpenFlow0x01.Message.FlowModMsg (SDN_OpenFlow0x01.from_flow prio flow) in
  let c_id = Controller.client_id_of_switch t sw_id in
  let table = LocalCompiler.(to_table (of_policy sw_id pol)) in
  let priority = ref 65536 in
  let open Deferred in
  send t c_id (0l, delete_flows) >>= fun _ ->
  ignore (List.map table ~f:(fun flow ->
    decr priority;
    send t c_id (0l, to_flow_mod !priority flow)))

let handler t app =
  let handler' = Async_NetKAT.run app in
  fun e ->
    let open Deferred in
    let nib = Controller.nib t in
    handler' nib e >>= fun (packet_outs, m_pol) ->
    let outs = List.map packet_outs ~f:(fun ((sw_id,_,_,_,_) as po) ->
      (* XXX(seliopou): xid *)
      let c_id = Controller.client_id_of_switch t sw_id in
      send t c_id (0l, packet_out_to_message po)) in
    let pols = match m_pol with
      | Some (pol) ->
        ignore (List.map ~how:`Parallel (Topology.get_switchids nib) ~f:(fun sw_id ->
          update_table_for t sw_id pol))
      | None ->
        let open NetKAT_Types in
        begin match e with
          | SwitchUp sw_id ->
            update_table_for t sw_id (Async_NetKAT.default app)
          | _ -> return ()
        end in
    ignore outs >>= fun _ ->
    ignore pols

let start app ?(port=6633) () =
  let open Async_OpenFlow.Platform.Trans in
  let stages = Controller.(features >=> topology >=> to_event) in

  Controller.create ~max_pending_connections ~port ()
  >>> fun t ->
    let events = run stages t (Controller.listen t) in
    Deferred.don't_wait_for (Pipe.iter events ~f:(handler t app))
