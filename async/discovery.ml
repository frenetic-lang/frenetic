open Core.Std
open Async.Std

exception Assertion_failed of string

module Switch = struct
  module Log = Async_OpenFlow.Log
  let tags = [("netkat", "topology.switch")]

  module Probe = struct
    cstruct probe_payload {
      uint64_t switch_id;
      uint32_t port_id
    } as big_endian

    (* XXX(seliopou): Watch out for this. The protocol in etheret packets has two
     * different meanings depending on the range of values that it falls into. If
     * anything weird happens with probe sizes, look here.
     *
     * This is not the protocol, but in fact the size.
     * *)
    let protocol = 0x05ff
    let mac = 0xffeabbadabbaL

    exception Wrong_type

    (* A probe consists of a switch_id and port_id, both represented as int64s
     * regardless of the underlying OpenFlow protocol's representation.
     * *)
    type t =
      { switch_id : int64
      ; port_id : int32
      }

    let marshal t b =
      set_probe_payload_switch_id b t.switch_id;
      set_probe_payload_port_id b t.port_id;
      sizeof_probe_payload

    let marshal' t =
      let b = Cstruct.create sizeof_probe_payload in
      ignore (marshal t b);
      b

    let parse b =
      { switch_id = get_probe_payload_switch_id b
      ; port_id = get_probe_payload_port_id b
      }

    let of_packet p =
      let open Packet in
      match p.nw with
        | Unparsable(proto, b)
          when proto = protocol -> parse b
        | _ -> raise Wrong_type

    let to_packet t =
      let open Packet in
      { dlSrc = mac
      ; dlDst = 0xffffffffffffL
      ; dlVlan = None
      ; dlVlanDei = false
      ; dlVlanPcp = 0x0
      ; nw = Unparsable(protocol, marshal' t)
      }
  end

  module SwitchMap = Map.Make(Int64)

  let guard app =
    let open NetKAT_Types in
    let open Async_NetKAT in
    let gpol = Filter(And(Neg(Test(EthSrc Probe.mac)),
                          Neg(Test(EthType Probe.protocol)))) in
    seq (create_static gpol) app

  let create () =
    let open NetKAT_Types in
    let open Async_NetKAT in
    (* Maps switch_ids to a set of ports that switch that haven't been matched
     * up with their other end yet. *)
    let pending = ref SwitchMap.empty in
    (* The default and static policy. This should be installed on all switches
     * and never change. *)
    let default = Seq(Filter(And(Test(EthType Probe.protocol),
                                 Test(EthSrc  Probe.mac))),
                      Mod(Location(Pipe("probe")))) in
    (* A pipe for topology events. *)
    let t_r, t_w = Pipe.create () in

    let handle_probe t switch_id port_id probe : Net.Topology.t Deferred.t =
      let open Probe in
      let open Net.Topology in
      (* XXX(seliopou): symmetric case *)
      pending := SwitchMap.change !pending probe.switch_id (function
        | None -> raise (Assertion_failed (Printf.sprintf
                    "Discovery.handle_probe: unknown switch %Lu" probe.switch_id));
        | Some(ports) -> Some(PortSet.remove probe.port_id ports));
      pending := SwitchMap.change !pending switch_id (function
        | None -> None (* It is possible for the controller to receive PacketIn
                          events from a switch before its probes are sent. It
                          however should not be possible to get a PacketIn
                          after a switch has disconnect. *)
        | Some(ports) -> Some(PortSet.remove port_id ports));
      let v = vertex_of_label t (Switch switch_id) in
      match next_hop t v port_id with
        | Some(e) ->
          let v, port_id' = edge_dst e in
          if not (probe.port_id = port_id')
            then raise (Assertion_failed (Printf.sprintf
                    "Discovery.handle_probe: probe inconsistent w/ topology"));
          return t
        | None ->
          Log.info ~tags "Create LinkUp: %Lu@%lu <=> %Lu@%lu"
            switch_id       port_id
            probe.switch_id probe.port_id;
          let t, v1 = add_vertex t (Switch switch_id) in
          let t, v2 = add_vertex t (Switch probe.switch_id) in
          let t, _  = add_edge t v1 port_id () v2 probe.port_id in
          let t, _  = add_edge t v2 probe.port_id () v1 port_id in
          let e1 = (switch_id, VInt.Int32 port_id) in
          let e2 = (probe.switch_id, VInt.Int32 probe.port_id) in
          Pipe.write t_w (LinkUp (e1, e2)) >>| fun _ -> t in

    let handler t w e : result Deferred.t =
      let open Net.Topology in
      match e with
        | PacketIn(p, sw_id, pt_id, bytes, len, buf) ->
          if not (p = "probe")
            then raise (Assertion_failed (Printf.sprintf
                "Discovery.handler: not listening to pipe \"%s\"" p));
          let open Packet in
          let pt_id' = VInt.get_int32 pt_id in
          begin match parse bytes with
            | { nw = Unparsable (dlTyp, bytes) } ->
              handle_probe !t sw_id pt_id' (Probe.parse bytes) >>| fun t' ->
                t := t'
            | _ -> return ()
          end >>= fun _ ->
          return None
        | SwitchUp sw_id ->
          Log.info ~tags "SwitchUp: %Lu" sw_id;
          t := fst (add_vertex !t (Switch sw_id));
          return None
        | SwitchDown sw_id ->
          Log.info ~tags "SwitchDown: %Lu" sw_id;
          t := remove_vertex !t (vertex_of_label !t (Switch sw_id));
          return None
        | PortUp (sw_id, pt_id) ->
          let pt_id' = VInt.get_int32 pt_id in
          Log.info ~tags "PortUp: %Lu@%lu" sw_id pt_id';
          pending := SwitchMap.change !pending sw_id (function
            | None -> Some(PortSet.singleton pt_id')
            | Some(ports) -> Some(PortSet.add pt_id' ports));
          let bytes = Packet.marshal
            Probe.(to_packet { switch_id = sw_id; port_id = pt_id' }) in
          let out = (sw_id, bytes, None, Some(pt_id), [SDN_Types.OutputPort(pt_id)]) in
          Pipe.write w out >>= fun _ ->
          return None
        | PortDown (sw_id, pt_id) ->
          let pt_id' = VInt.get_int32 pt_id in
          Log.info ~tags "PortDown: %Lu@%lu" sw_id pt_id';
          pending := SwitchMap.change !pending sw_id (function
            | None -> None
            | Some(ports) -> Some(PortSet.remove pt_id' ports));
          let v = vertex_of_label !t (Switch sw_id) in
          t := remove_endpoint !t (v, pt_id');
          return None
        | LinkUp ((sw1, pt1), (sw2, pt2)) ->
          Log.info ~tags "LinkUp: %Lu@%lu <=> %Lu@%lu"
            sw1 (VInt.get_int32 pt1)
            sw2 (VInt.get_int32 pt2);
          return None
        | HostUp _
        | HostDown _
        | Query _ ->
          return None in

    let app = create ~pipes:(PipeSet.singleton "probe") default handler in
    (t_r, app)

end

let guard app =
  Switch.guard app

let create () =
  Switch.create ()
  (* Async_NetKAT.union (Host.create ()) (Switch.create()) *)
