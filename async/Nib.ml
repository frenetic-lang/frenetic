open Core.Std

module Probe = struct

  cstruct nib_payload {
    uint64_t switch_id;
    uint32_t port_id
  } as big_endian

  (* XXX(seliopou): Watch out for this. The protocol in etheret packets has two
   * different meanings depending on the range of values that it falls into. If
   * anything weird happens with probe sizes, look here.
   *
   * This is not the protocol, but in fact the size.
   * *)
  let protocol = 0x05fb
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
    set_nib_payload_switch_id b t.switch_id;
    set_nib_payload_port_id b t.port_id;
    sizeof_nib_payload

  let marshal' t =
    let b = Cstruct.create sizeof_nib_payload in
    ignore (marshal t b);
    b

  let parse b =
    { switch_id = get_nib_payload_switch_id b
    ; port_id = get_nib_payload_port_id b
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

module Node = struct
  type t =
    | Switch of SDN_Types.switchId
    | Host of Packet.dlAddr * Packet.nwAddr

  let compare = Pervasives.compare

  let to_string t = match t with
    | Switch(sw_id)       -> Printf.sprintf "switch %Lu" sw_id
    | Host(dlAddr, nwAddr) -> Printf.sprintf "host %s/%s"
        (Packet.string_of_nwAddr nwAddr)
        (Packet.string_of_dlAddr dlAddr)

  let parse_dot _ _ = failwith "NYI: Node.parse_dot"
  let parse_gml _ = failwith "NYI: Node.parse_dot"
end

module Edge = struct
  type t = unit

  let compare = Pervasives.compare

  let to_string () = "()"
  let default = ()

  let parse_dot _ = failwith "NYI: Edge.parse_dot"
  let parse_gml _ = failwith "NYI: Edge.parse_dot"
end

module Net = Network.Make(Node)(Edge)

module Protocol : sig
  open Async.Std

  type t

  module SwitchMap : Map.S
    with type Key.t = SDN_Types.switchId

  module PortSet : Set.S
    with type Elt.t = Int32.t

  val create
    : ?nib:Net.Topology.t
    -> unit
    -> t

  val state
    : t -> Net.Topology.t

  val setup_probe
    : t
    -> send:(OpenFlow0x01.Message.t -> unit Deferred.t)
    -> OpenFlow0x01.SwitchFeatures.t
    -> t Deferred.t

  val setup_arp
    : t
    -> send:(OpenFlow0x01.Message.t -> unit Deferred.t)
    -> t Deferred.t

  val remove_switch
    : t
    -> SDN_Types.switchId
    -> t

  val handle_port_status
    : t
    -> send:(OpenFlow0x01.Message.t -> unit Deferred.t)
    -> SDN_Types.switchId
    -> OpenFlow0x01.PortStatus.t
    -> t Deferred.t

  val handle_probe
    : t
    -> send:(OpenFlow0x01.Message.t -> unit Deferred.t)
    -> SDN_Types.switchId
    -> OpenFlow0x01.PacketIn.t
    -> (t * OpenFlow0x01.PacketIn.t option) Deferred.t

  val handle_arp
    : t
    -> send:(OpenFlow0x01.Message.t -> unit Deferred.t)
    -> SDN_Types.switchId
    -> OpenFlow0x01.PacketIn.t
    -> (t * OpenFlow0x01.PacketIn.t option) Deferred.t

end = struct
  open Async.Std

  module SwitchMap = Map.Make(Int64)
  module PortSet = Set.Make(Int32)

  module Log = Async_OpenFlow_Log
  let tags = [("openflow", "topology")]

  type t =
    { nib : Net.Topology.t
    ; pending : PortSet.t SwitchMap.t
    }

  let create ?nib () =
    let nib = match nib with
      | Some(n) -> n
      | None -> Net.Topology.empty () in
    { nib; pending = SwitchMap.empty }

  let state (t : t) : Net.Topology.t = t.nib

  open OpenFlow0x01

  let port_useable descr =
    let open PortDescription in
     if descr.config.PortConfig.down
       then false
       else not (descr.state.PortState.down)

  let add_ports_edge (nib:Net.Topology.t) n1 (p1:int32) n2 (p2:int32) =
    let open Net.Topology in
    let nib, v1 = try (nib, vertex_of_label nib n1)
      with Not_found -> add_vertex nib n1 in
    let nib, v2 = try (nib, vertex_of_label nib n2)
      with Not_found -> add_vertex nib n2 in
    let nib, _ = Net.Topology.add_edge nib v1 p1 () v2 p2 in
    let nib, _ = Net.Topology.add_edge nib v2 p2 () v1 p1 in
    nib

  let add_host_edge (nib:Net.Topology.t) s (p:int32) h =
    add_ports_edge nib s p h 0l

  let next_hop_via (nib:Net.Topology.t) n1 p1 =
    let open Net.Topology in
    try
      let v1 = vertex_of_label nib n1 in
      Misc.map_option edge_dst (next_hop nib v1 p1)
    with Not_found -> None

  let next_switch_hop_via (nib:Net.Topology.t) n1 p1 =
    match next_hop_via nib n1 p1 with
      | Some(v2, p2) ->
        begin match Net.Topology.vertex_to_label nib v2 with
          | Node.Switch(sw_id) -> Some(sw_id, p2)
          | Node.Host _ -> None
        end
      | None -> None

  let remove_endpoint (nib0 : Net.Topology.t) (n1, p1) =
    let open Net.Topology in
    let v1 = vertex_of_label nib0 n1 in
    match next_hop nib0 v1 p1 with
      | Some(e) ->
        let nib1 = remove_endpoint nib0 (edge_src e) in
        let nib2 = remove_endpoint nib1 (edge_dst e) in
        nib2
      | None -> nib0

  let receive_probe t switch_id port_id probe =
    let open Probe in
    let ports = try SwitchMap.find_exn t.pending probe.switch_id
      with Not_found -> PortSet.empty in
    let nib = match next_switch_hop_via t.nib (Node.Switch switch_id) port_id with
      | Some(sw', p') ->
        assert (sw' = probe.switch_id);
        assert (p'  = probe.port_id);
        t.nib
      | None ->
        Log.info ~tags "link(add): %Lu@%lu <=> %Lu@%lu"
          switch_id       port_id
          probe.switch_id probe.port_id;
        add_ports_edge t.nib
          (Node.Switch switch_id)       port_id
          (Node.Switch probe.switch_id) probe.port_id in
    { nib
    ; pending = SwitchMap.add t.pending switch_id (PortSet.remove ports probe.port_id)
    }

  let handle_arp t switch_id port arp =
    let ports = try SwitchMap.find_exn t.pending switch_id
      with Not_found -> PortSet.empty in
    let open Packet in
    match arp with
      | Arp.Query(dlSrc, nwSrc, _)
      | Arp.Reply(dlSrc, nwSrc, _, _) ->
        let h, s = Node.Host(dlSrc, nwSrc), Node.Switch(switch_id) in
        begin match next_switch_hop_via t.nib h 0l with
          | Some (sw, pt) ->
            (* XXX(seliopou): For now, the code assumes that the entire network
             * is under the management of the controller, and that a host will
             * go down before it comes up at another port, i.e., each MAC/IP is
             * connected to a single port at any given time.
             * *)
            Log.info ~tags "arp: packet for known host %s@0 <=> %Lu@%lu seen at %Lu@%lu"
              (Node.to_string h)
              sw        pt
              switch_id port
          | None ->
            Log.info ~tags "link(add): %Lu@%lu <=> %s"
              switch_id port
              (Node.to_string h)
        end;
        { nib = add_host_edge t.nib s port h
        ; pending = SwitchMap.add t.pending switch_id (PortSet.remove ports port)
        }

  let add_port t ~send ~switch_id port =
    let port_id = Int32.of_int_exn (port.PortDescription.port_no) in
    let set = match SwitchMap.find t.pending switch_id with
      | Some(set0) -> PortSet.add set0 port_id
      | None -> PortSet.singleton port_id in
    let open OpenFlow0x01_Core in
    let output_payload =
      NotBuffered (Packet.marshal Probe.(to_packet { switch_id; port_id })) in
    let apply_actions = [Output (PhysicalPort(Int32.to_int_exn port_id))] in
    (* XXX(seliopou): In addition to sending the initial probe, this function
     * should also start a timer that checks that the probe has been received by
     * the controller and, if it has not, should send another probe.
     *
     * Until everything else is in working order, this will do.
     * *)
    send (Message.PacketOutMsg { output_payload; port_id = None; apply_actions })
    >>| fun _ -> { t
      with pending = SwitchMap.add t.pending switch_id set }

  let handle_remove_port t ~switch_id port =
    let ports = try SwitchMap.find_exn t.pending switch_id
      with Not_found -> PortSet.empty in
    let p1 = Int32.of_int_exn (port.OpenFlow0x01.PortDescription.port_no) in
    begin match next_hop_via t.nib (Node.Switch switch_id) p1 with
      | Some(v,p2) ->
        let n = Net.Topology.vertex_to_label t.nib v in
        Log.info ~tags "link(remove): %Lu@%lu <=> %s@%lu"
          switch_id          p1
          (Node.to_string n) p2
      | _ -> ()
    end;
    { nib = remove_endpoint t.nib (Node.Switch switch_id, p1)
    ; pending = SwitchMap.add t.pending switch_id (PortSet.remove ports p1)
    }

  let setup_probe t ~send feats =
    (* XXX(seliopou): Depending on how the switch is configured, these may not be
     * strictly necessary. Also, see the comment below for notes on timing.
     *
     * Also, mind the note on timing in `add_port`.
     * *)
    let probe_rule =
      let open Message in
      let open OpenFlow0x01_Core in
      let nib_probe = { match_all
        with dlTyp = Some(Probe.protocol)
           ; dlSrc = Some(Probe.mac)
           ; dlDst = Some(0xffffffffffffL)
      } in
      FlowModMsg (add_flow 65535 nib_probe  [Output(Controller(512))]) in
    let open SwitchFeatures in
    let switch_id = feats.switch_id in
    let ports = List.filter feats.ports ~f:port_useable in
    Log.info ~tags "switch(add): %Lu" switch_id;
    send probe_rule >>= fun _ ->
    Deferred.List.fold ports ~init:t ~f:(add_port ~send ~switch_id)
    >>| fun t' ->
        let nib, _ = Net.Topology.add_vertex t'.nib (Node.Switch feats.switch_id) in
        { t' with nib }

  let setup_arp t ~send =
    (* XXX(seliopou): Depending on how the switch is configured, these may not be
     * strictly necessary. Also, see the comment below for notes on timing.
     * *)
    let arp_rule =
      let open Message in
      let open OpenFlow0x01_Core in
      let arp_packet = { match_all with dlTyp = Some(0x0806) } in
      FlowModMsg (add_flow 65535 arp_packet [Output(Controller(512))]) in
    send arp_rule >>= fun _ ->
    return t

  let remove_switch t switch_id =
    let open Net.Topology in
    Log.info ~tags "switch(remove): %Lu" switch_id;
    let v = vertex_of_label t.nib (Node.Switch switch_id) in
    { t with nib = remove_vertex t.nib v }

  let handle_port_status t ~send switch_id port_status =
    let open PortStatus in
    match port_status.reason, port_useable port_status.desc with
      | ChangeReason.Add, true
      | ChangeReason.Modify, true ->
        add_port t ~switch_id port_status.desc ~send
      | ChangeReason.Delete, _
      | ChangeReason.Modify, false ->
        return (handle_remove_port t ~switch_id port_status.desc)
      | _ ->
        return t

  let handle_probe t ~send switch_id pi =
    let open OpenFlow0x01_Core in
    let open Packet in
    match OpenFlow0x01_Core.parse_payload pi.input_payload with
      | { dlDst = 0xffffffffffffL; dlSrc; nw = Unparsable (dlTyp, bytes) }
        when dlTyp = Probe.protocol && dlSrc = Probe.mac ->
          let t = receive_probe t switch_id (Int32.of_int_exn pi.port) (Probe.parse bytes) in
          return (t, None)
      | _ -> return (t, Some(pi))

  let handle_arp t ~send switch_id pi =
    let open OpenFlow0x01_Core in
    let open Packet in
    match OpenFlow0x01_Core.parse_payload pi.input_payload with
      | { nw = Arp arp } ->
        return (handle_arp t switch_id (Int32.of_int_exn pi.port) arp, Some(pi))
      | _ -> return (t, Some(pi))

end
