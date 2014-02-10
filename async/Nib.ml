open Core.Std
open Topology


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
    : ?nib:Topology.t
    -> unit
    -> t

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
  module PortSet = Set.Make(Int64)

  module Log = Async_OpenFlow_Log
  let tags = [("openflow", "topology")]

  type t =
    { nib : Topology.t
    ; pending : PortSet.t SwitchMap.t
    }

  let create ?nib () =
    let nib = match nib with
      | Some(n) -> n
      | None -> Topology.empty in
    { nib; pending = SwitchMap.empty }

  open OpenFlow0x01

  let port_useable descr =
    let open PortDescription in
     if descr.config.PortConfig.down
       then false
       else not (descr.state.PortState.down)

  let add_ports_edge (nib:Topology.t) n1 (p1:int64) n2 (p2:int64) =
    Topology.(add_ports_edge
      (add_ports_edge nib n1 p1 n2 p2)
      n2 p2 n1 p1)

  let add_host_edge (nib:Topology.t) s (p:int64) h =
    add_ports_edge nib s p h 0L

  let next_hop_via nib n port_id =
    try Some(Topology.next_hop_via nib n port_id)
      with Topology.NotFound(_) -> None

  let next_switch_hop_via nib sw port_id =
    match next_hop_via nib sw port_id with
      | Some(l, Node.Switch sw) -> Some(l.Link.dstport, sw)
      | _ -> None

  let next_host_hop_via nib h =
    match next_hop_via nib h 0L with
      | Some(l, Node.Switch sw) -> Some(l.Link.dstport, sw)
      | _ -> None

  let remove_port nib n p =
    match next_hop_via nib n p with
      | Some(l, h') ->
        Topology.(remove_port
          (remove_port nib n p)
          h' l.Link.dstport)
      | None -> nib

  let receive_probe t switch_id port_id probe =
    let open Probe in
    let ports = try SwitchMap.find_exn t.pending probe.switch_id
      with Not_found -> PortSet.empty in
    let n1, n2 = Node.Switch switch_id, Node.Switch probe.switch_id in
    let nib = match next_switch_hop_via t.nib n1 port_id with
      | Some(p', sw') ->
        assert (sw' = probe.switch_id);
        assert (p'  = probe.port_id);
        t.nib
      | None ->
        Log.info ~tags "link(add): %Lu@%Lu <=> %Lu@%Lu"
          switch_id       port_id
          probe.switch_id probe.port_id;
        add_ports_edge t.nib
          n1 port_id
          n2 probe.port_id in
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
        let host_str = Printf.sprintf "%s/%s"
          (Packet.string_of_ip nwSrc)
          (Packet.string_of_mac dlSrc) in
        let h, s = Node.Host(host_str, dlSrc, nwSrc), Node.Switch(switch_id) in
        begin match next_host_hop_via t.nib h with
          | Some (pt, sw) ->
            (* XXX(seliopou): For now, the code assumes that the entire network
             * is under the management of the controller, and that a host will
             * go down before it comes up at another port, i.e., each MAC/IP is
             * connected to a single port at any given time.
             * *)
            Log.info ~tags "arp: packet for known host %s <=> %Lu@%Lu seen at %Lu@%Lu"
              host_str
              sw        pt
              switch_id port
          | None ->
            Log.info ~tags "link(add): %Lu@%Lu <=> %s"
              switch_id port
              host_str
        end;
        { nib = add_host_edge t.nib s port h
        ; pending = SwitchMap.add t.pending switch_id (PortSet.remove ports port)
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

  let handle_remove_port t ~switch_id port =
    let ports = try SwitchMap.find_exn t.pending switch_id
      with Not_found -> PortSet.empty in
    let p = Int64.of_int (port.OpenFlow0x01.PortDescription.port_no) in
    begin match next_hop_via t.nib (Node.Switch switch_id) p with
      | Some(l, Node.Switch s2) ->
        Log.info ~tags "link(remove): %Lu@%Lu <=> %Lu@%Lu"
          switch_id p
          s2        l.Link.dstport
      | Some(l, Node.Host(h_str, dlAddr, nwAddr)) ->
        Log.info ~tags "link(remove): %Lu@%Lu <=> %s"
          switch_id l.Link.dstport
          h_str
      | _ -> ()
    end;
    { nib = remove_port t.nib (Node.Switch switch_id) p
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
      with nib = Topology.add_switch t'.nib feats.switch_id }

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
    Log.info ~tags "switch(remove): %Lu" switch_id;
    { t with nib = Topology.remove_switch t.nib switch_id }

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
          let t = receive_probe t switch_id (Int64.of_int pi.port) (Probe.parse bytes) in
          return (t, None)
      | _ -> return (t, Some(pi))

  let handle_arp t ~send switch_id pi =
    let open OpenFlow0x01_Core in
    let open Packet in
    match OpenFlow0x01_Core.parse_payload pi.input_payload with
      | { nw = Arp arp } ->
        return (handle_arp t switch_id (Int64.of_int pi.port) arp, Some(pi))
      | _ -> return (t, Some(pi))

end
