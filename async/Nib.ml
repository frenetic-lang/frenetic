open Core.Std

module State = struct

  type host = unit
  type switch = SDN_Types.switchId

  type port = int64
    with sexp

  type node =
    | Host of host sexp_opaque
    | Switch of switch sexp_opaque
    with sexp

  module NodeSet = Set.Make(struct
    type t = node with sexp
    let compare = Pervasives.compare
  end)

  module PortMap = Map.Make(struct
    type t = (node * port) with sexp
    let compare = Pervasives.compare
  end)

  type t =
    { nodes : NodeSet.t
    ; ends : (node * port) PortMap.t
    }

  let empty =
    { nodes = NodeSet.empty; ends = PortMap.empty }

  let add_node (t : t) (n : node) : t =
    assert (not (NodeSet.mem t.nodes n));
    { t with nodes = NodeSet.add t.nodes n }

  let add_link (t : t) n1 p1 n2 p2 : t =
    assert (NodeSet.mem t.nodes n1);
    assert (NodeSet.mem t.nodes n2);

    let nodes = NodeSet.(union t.nodes (of_list [n1; n2])) in
    let ends = PortMap.add t.ends (n1, p1) (n2, p2) in
    let ends = PortMap.add ends   (n2, p2) (n1, p1) in
    { nodes; ends }

  let remove_node (t : t) (n : node) : t =
    assert (NodeSet.mem t.nodes n);
    { nodes = NodeSet.remove t.nodes n
    ; ends = PortMap.filter t.ends ~f:(fun ~key:(n0, p0) ~data:(n1, p0) ->
        not ((n0 = n) || (n1 = n))) }

  let remove_port (t : t) (n : node) (p : port) : t =
    match PortMap.find t.ends (n, p) with
      | Some(l) ->
        { t with ends = PortMap.(remove (remove t.ends (n, p)) l) }
      | None ->
        t

  let has_port (t : t) (n : node) (p : port) : (node * port) option =
    PortMap.find t.ends (n, p)

end

module Probe = struct

  cstruct nib_payload {
    uint64_t switch_id;
    uint64_t port_id
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
    ; port_id : int64
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

module Protocol : sig
  open Async.Std

  type t

  module SwitchMap : Map.S
    with type Key.t = SDN_Types.switchId

  module PortSet : Set.S
    with type Elt.t = Int64.t

  val create
    : ?nib:State.t
    -> unit
    -> t

  val add_switch
    : t
    -> send:(OpenFlow0x01.Message.t -> unit Deferred.t)
    -> OpenFlow0x01.SwitchFeatures.t
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

  val handle_packet_in
    : t
    -> send:(OpenFlow0x01.Message.t -> unit Deferred.t)
    -> SDN_Types.switchId
    -> OpenFlow0x01.PacketIn.t
    -> (t * OpenFlow0x01.PacketIn.t option) Deferred.t

end = struct
  open Async.Std

  module SwitchMap = Map.Make(Int64)
  module PortSet = Set.Make(Int64)

  type t =
    { nib : State.t
    ; pending : PortSet.t SwitchMap.t
    }

  let create ?nib () =
    let nib = match nib with
      | Some(n) -> n
      | None -> State.empty in
    { nib; pending = SwitchMap.empty }

  open OpenFlow0x01

  (* XXX(seliopou): Depending on how the switch is configured, these may not be
   * strictly necessary. Also, see the comment below for notes on timing.
   * *)
  let setup_flows =
    let open Message in
    let open OpenFlow0x01_Core in
    let nib_probe = { match_all
      with dlTyp = Some(Probe.protocol)
         ; dlSrc = Some(Probe.mac)
         ; dlDst = Some(0xffffffffffffL)
    } in
    [ FlowModMsg (add_flow 65535 nib_probe  [Output(Controller(512))])
    ]

  let port_useable descr =
    let open PortDescription in
     if descr.config.PortConfig.down
       then false
       else not (descr.state.PortState.down)

  let receive_probe t switch_id port_id probe =
    let open Probe in
    let ports = try SwitchMap.find_exn t.pending probe.switch_id
      with Not_found -> PortSet.empty in
    let n1, n2 = State.Switch switch_id, State.Switch probe.switch_id in
    let nib = match State.has_port t.nib n1 port_id with
      | Some(l) ->
        assert (l = (n2, probe.port_id));
        t.nib
      | None ->
        State.add_link t.nib n1 port_id n2 probe.port_id in
    { nib
    ; pending = SwitchMap.add t.pending switch_id (PortSet.remove ports probe.port_id)
    }

  let add_port t ~send ~switch_id port =
    let port_no = port.PortDescription.port_no in
    let port_id = Int64.of_int port_no in
    let set = match SwitchMap.find t.pending switch_id with
      | Some(set0) -> PortSet.add set0 port_id
      | None -> PortSet.singleton port_id in
    let open OpenFlow0x01_Core in
    let output_payload =
      NotBuffered (Packet.marshal Probe.(to_packet { switch_id; port_id })) in
    let apply_actions = [Output (PhysicalPort(port_no))] in
    (* XXX(seliopou): In addition to sending the initial probe, this function
     * should also start a timer that checks that the probe has been received by
     * the controller and, if it has not, should send another probe.
     *
     * Until everything else is in working order, this will do.
     * *)
    send (Message.PacketOutMsg { output_payload; port_id = None; apply_actions })
    >>| fun _ -> { t
      with pending = SwitchMap.add t.pending switch_id set }

  let remove_port t ~switch_id port =
    let port_no = port.OpenFlow0x01.PortDescription.port_no in
    { t with nib = State.remove_port t.nib (State.Switch switch_id) (Int64.of_int port_no) }

  let add_switch t ~send feats =
    let open SwitchFeatures in
    let switch_id = feats.switch_id in
    let ports = List.filter feats.ports ~f:port_useable in
    Deferred.List.map ~how:`Parallel setup_flows ~f:send >>= fun _ ->
    Deferred.List.fold ports ~init:t ~f:(add_port ~send ~switch_id)
    >>| fun t' -> { t'
      with nib = State.add_node t'.nib (State.Switch feats.switch_id) }

  let remove_switch t switch_id =
    { t with nib = State.remove_node t.nib (State.Switch switch_id) }

  let handle_port_status t ~send switch_id port_status =
    let open PortStatus in
    match port_status.reason, port_useable port_status.desc with
      | ChangeReason.Add, true
      | ChangeReason.Modify, true ->
        add_port t ~switch_id port_status.desc ~send
      | ChangeReason.Delete, _
      | ChangeReason.Modify, false ->
        return (remove_port t ~switch_id port_status.desc)
      | _ ->
        return t

  let handle_packet_in t ~send switch_id pi =
    let open OpenFlow0x01_Core in
    let open Packet in
    match OpenFlow0x01_Core.parse_payload pi.input_payload with
      | { dlDst = 0xffffffffffffL; dlSrc; nw = Unparsable (dlTyp, bytes) }
        when dlTyp = Probe.protocol && dlSrc = Probe.mac ->
          let t = receive_probe t switch_id (Int64.of_int pi.port) (Probe.parse bytes) in
          return (t, None)
      | _ -> return (t, Some(pi))

end
