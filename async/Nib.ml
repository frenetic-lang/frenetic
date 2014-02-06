open Core.Std

module State = struct

  type host = { dl : Packet.dlAddr; nw : Packet.nwAddr }
  type switch = SDN_Types.switchId

  type port = int64
    with sexp

  type node =
    | Host of host sexp_opaque
    | Switch of switch sexp_opaque
    with sexp

  type end_ =
    | SP of (switch * port) sexp_opaque
    | H of host sexp_opaque
    with sexp

  module NodeSet = Set.Make(struct
    type t = node with sexp
    let compare = Pervasives.compare
  end)

  module EndMap = Map.Make(struct
    type t = end_ with sexp
    let compare = Pervasives.compare
  end)

  type t =
    { nodes : NodeSet.t
    ; ends : end_ EndMap.t
    }

  let empty =
    { nodes = NodeSet.empty; ends = EndMap.empty }

  let _add_node (t : t) (n : node) : t =
    { t with nodes = NodeSet.add t.nodes n }

  let _link_invariant (t : t) (e : end_) : bool =
    match e with
      | SP (sw, _) -> NodeSet.mem t.nodes (Switch sw)
      | H h -> NodeSet.mem t.nodes (Host h)

  let _add_link (t : t) (e1 : end_) (e2 : end_) : t =
    assert (_link_invariant t e1);
    assert (_link_invariant t e2);
    let ends = EndMap.add t.ends e1 e2 in
    let ends = EndMap.add ends   e2 e1 in
    { t with ends }

  let add_switch (t : t) (sw : switch) : t =
    let n = Switch sw in
    _add_node t n

  let add_host (t : t) dl nw (sw : switch) (p : port) : t =
    let n = Host { dl; nw } in
    let e1, e2 = H { dl; nw }, SP(sw, p) in
    _add_link (_add_node t n) e1 e2

  let add_link (t : t) sw1 p1 sw2 p2 : t =
    let e1, e2 = SP (sw1, p1), SP (sw2, p2) in
    _add_link t e1 e2

  let remove_switch (t : t) (sw : switch) : t =
    let n = Switch sw in
    assert (NodeSet.mem t.nodes n);
    { nodes = NodeSet.remove t.nodes n
    ; ends = EndMap.filter t.ends ~f:(fun ~key ~data ->
        match key, data with
          | SP (sw', _), _
          | _, SP (sw', _)
            when sw = sw' -> false
          | _ -> true)
    }

  let remove_if_host (t : t) (e : end_) : t =
    match e with
      | SP _ -> t
      | H h -> { t with nodes = NodeSet.remove t.nodes (Host h) }

  let remove_port (t : t) (sw : switch) (p : port) : t =
    let e = SP (sw, p) in
    match EndMap.find t.ends e with
      | Some(l) ->
        { remove_if_host t l with
          ends = EndMap.(remove (remove t.ends e) l) }
      | None ->
        t

  let other_end_is_switch (t : t) (sw : switch) (p : port) =
    match EndMap.find t.ends (SP (sw, p)) with
      | Some(SP sw_p) -> Some(sw_p)
      | _ -> None

  let other_end_of_host (t : t) dl nw : (switch * port) option =
    match EndMap.find t.ends (H { dl; nw }) with
      | Some(SP sw_p) -> Some sw_p
      | Some(H _) -> assert false
      | None -> None

  let other_end_for_switch (t : t) (sw : switch) (p : port) =
    EndMap.find t.ends (SP (sw, p))

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

  val setup_probe
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

  val handle_probe
    : t
    -> send:(OpenFlow0x01.Message.t -> unit Deferred.t)
    -> SDN_Types.switchId
    -> OpenFlow0x01.PacketIn.t
    -> (t * OpenFlow0x01.PacketIn.t option) Deferred.t

end = struct
  open Async.Std

  module SwitchMap = Map.Make(Int64)
  module PortSet = Set.Make(Int64)

  module Log = Async_OpenFlow_Log
  let tags = [("openflow", "topology")]

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

  let port_useable descr =
    let open PortDescription in
     if descr.config.PortConfig.down
       then false
       else not (descr.state.PortState.down)

  let receive_probe t switch_id port_id probe =
    let open Probe in
    let ports = try SwitchMap.find_exn t.pending probe.switch_id
      with Not_found -> PortSet.empty in
    let nib = match State.other_end_is_switch t.nib switch_id port_id with
      | Some(l) ->
        assert (l = (probe.switch_id, probe.port_id));
        t.nib
      | None ->
        Log.info ~tags "link(add): %Lu@%Lu <=> %Lu@%Lu"
          switch_id       port_id
          probe.switch_id probe.port_id;
        State.add_link t.nib switch_id port_id probe.switch_id probe.port_id in
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
    let ports = try SwitchMap.find_exn t.pending switch_id
      with Not_found -> PortSet.empty in
    let p = Int64.of_int (port.OpenFlow0x01.PortDescription.port_no) in
    let open State in
    begin match State.other_end_for_switch t.nib switch_id p with
      | Some(SP(s2, p2)) ->
        Log.info ~tags "link(remove): %Lu@%Lu <=> %Lu@%Lu"
          switch_id p
          s2        p2
      | Some(H { dl; nw }) ->
        Log.info ~tags "link(remove): %Lu@%Lu <=> %s/%s"
          switch_id                 p
          (Packet.string_of_ip nw) (Packet.string_of_mac dl)
      | None -> ()
    end;
    { nib = State.remove_port t.nib switch_id p
    ; pending = SwitchMap.add t.pending switch_id (PortSet.remove ports p)
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
    >>| fun t' -> { t'
      with nib = State.add_switch t'.nib feats.switch_id }

  let remove_switch t switch_id =
    Log.info ~tags "switch(remove): %Lu" switch_id;
    { t with nib = State.remove_switch t.nib switch_id }

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

  let handle_probe t ~send switch_id pi =
    let open OpenFlow0x01_Core in
    let open Packet in
    match OpenFlow0x01_Core.parse_payload pi.input_payload with
      | { dlDst = 0xffffffffffffL; dlSrc; nw = Unparsable (dlTyp, bytes) }
        when dlTyp = Probe.protocol && dlSrc = Probe.mac ->
          let t = receive_probe t switch_id (Int64.of_int pi.port) (Probe.parse bytes) in
          return (t, None)
      | _ -> return (t, Some(pi))

end
