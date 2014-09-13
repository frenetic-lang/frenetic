open Core.Std
open Async.Std

module Flip = struct

  type t =
    { (* [enabled] is a boolean recording the state of the [Flip] application.
         When true the application should take on the value of [policy].
         When false, ti should take on the value [Filter False]. *)
      mutable enabled : bool;
      (* [e_value] is the value that the application should use when the app is
         in an enabled state. *)
      value: NetKAT_Types.pred;
      (* [update_bus] is the bus that all instances of the application subscribe
       * to for asynchronous value updates. *)
      update_bus : NetKAT_Types.pred Bus.t;
    }

  let create (value : 'a) =
    let open Async_NetKAT in
    let update_bus = Bus.create ~can_subscribe_after_start:true in
    let app = Pred.create_async NetKAT_Types.False (fun update () ->
      ignore (Bus.subscribe_exn update_bus ~f:(Pipe.write_without_pushback update));
      fun nib e -> return None)
    in
    { enabled = false; value; update_bus }, app

  let enable t =
    if t.enabled then
      return ()
    else begin
      t.enabled <- true;
      Bus.start t.update_bus;
      Bus.write t.update_bus t.value;
      Bus.flushed t.update_bus
    end

  let disable t =
    if not t.enabled then
      return ()
    else begin
      t.enabled <- false;
      Bus.write t.update_bus NetKAT_Types.False;
      Bus.flushed t.update_bus
    end
end

exception Assertion_failed of string

module Net = Async_NetKAT.Net

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

  type t =
    { (* *)
      flip_ctl : Flip.t;
      (* *)
      flip_app : Async_NetKAT.Pred.t;
      (* *)
      edge_app : (Net.Topology.t ref, (SDN_Types.switchId * SDN_Types.portId) list) Async_NetKAT.Raw.t;
      (* *)
      pkt_outs : (SDN_Types.switchId * SDN_Types.pktOut) Pipe.Writer.t;
      (* *)
      mutable probe_period : Time.Span.t;
      (* *)
      mutable loop : unit Deferred.t
    }

  let enabled t =
    t.flip_ctl.Flip.enabled

  let to_out switch_id port_id =
    let bytes = Packet.marshal
      Probe.(to_packet { switch_id; port_id = port_id }) in
    let action = SDN_Types.(Output(Physical(port_id))) in
    (switch_id, (SDN_Types.NotBuffered(bytes), Some(port_id), [action]))

  let loop t =
    Deferred.repeat_until_finished () (fun () ->
      if t.flip_ctl.Flip.enabled then
        Clock.after t.probe_period
        >>= fun () ->
        print_endline "IM IN THE LOOP\n";
        Deferred.List.iter (Async_NetKAT.default t.edge_app) ~f:(fun (sw_id, pt_id) ->
          Pipe.write t.pkt_outs (to_out sw_id pt_id))
        >>| fun _ -> `Repeat ()
      else
        return (`Finished ()))

  let handle_probe (t:Net.Topology.t) switch_id port_id probe =
    let open Probe in
    let open Net.Topology in
    let open Async_NetKAT in
    (* It is possible for the controller to receive PacketIn events from a
     * switch before its probes are sent. It however should not be possible to
     * get a PacketIn after a switch has disconnect. *)
    let v = vertex_of_label t (Switch switch_id) in
    match next_hop t v port_id with
      | Some(e) ->
        let v, port_id' = edge_dst e in
        begin if not (probe.port_id = port_id') then
          raise (Assertion_failed (Printf.sprintf
                  "Discovery.handle_probe: probe inconsistent w/ topology"))
        end;
        t, []
      | None ->
        Log.info ~tags "[topology.switch] ↑ { switch = %Lu; port %lu }, { switch = %Lu; port = %lu }"
          switch_id       port_id
          probe.switch_id probe.port_id;
        let t, v1 = add_vertex t (Switch switch_id) in
        let t, v2 = add_vertex t (Switch probe.switch_id) in
        let t, _  = add_edge t v1 port_id () v2 probe.port_id in
        let t, _  = add_edge t v2 probe.port_id () v1 port_id in
        let e1 = (switch_id, port_id) in
        let e2 = (probe.switch_id, probe.port_id) in
        t, [NetKAT_Types.LinkUp(e1, e2)]

  let stage t nib e =
    let open Net.Topology in
    let open NetKAT_Types in
    let open Async_NetKAT in
    match e with
      | PacketIn("probe", sw_id, pt_id, payload, len) when enabled t ->
        let open Packet in
        begin match parse (SDN_Types.payload_bytes payload) with
          | { nw = Unparsable (dlTyp, bytes) } when dlTyp = Probe.protocol ->
            let nib', es =  handle_probe !nib sw_id pt_id (Probe.parse bytes) in
            nib := nib';
            return es
          | _ -> return []
        end
      | SwitchUp sw_id ->
        Log.info ~tags "[topology.switch] ↑ { switch = %Lu }" sw_id;
        nib := fst (add_vertex !nib (Switch sw_id));
        return [e]
      | SwitchDown sw_id ->
        Log.info ~tags "[topology.switch] ↓ { switch = %Lu }" sw_id;
        nib := remove_vertex !nib (vertex_of_label !nib (Switch sw_id));
        return [e]
      | PortUp (sw_id, pt_id) ->
        Log.info ~tags "[topology.switch] ↑ { switch = %Lu; port = %lu }" sw_id pt_id;
        nib := add_port !nib (vertex_of_label !nib (Switch sw_id)) pt_id;
        Pipe.write t.pkt_outs (to_out sw_id pt_id)
        >>| fun () -> [e]
      | PortDown (sw_id, pt_id) ->
        Log.info ~tags "[topology.switch] ↓ { switch = %Lu; port = %lu }" sw_id pt_id;
        let v = vertex_of_label !nib (Switch sw_id) in
        let mh = next_hop !nib v pt_id in
        let es = begin match mh with
          | None -> []
          | Some(edge) ->
            let (v2, pt_id2) = edge_dst edge in
            begin match vertex_to_label !nib v2 with
              | Switch(sw_id2) ->
                nib := remove_endpoint !nib (v, pt_id);
                Log.info ~tags "[topology.switch] ↓ { switch = %Lu; port %lu }, { switch = %Lu; port = %lu }"
                  sw_id  pt_id
                  sw_id2 pt_id2;
                [LinkDown((sw_id, pt_id), (sw_id2, pt_id2))]
              | Host _ -> []
            end
        end in
        return es
      | LinkUp ((sw1, pt1), (sw2, pt2)) ->
        assert false
      | LinkDown ((sw1, pt1), (sw2, pt2)) ->
        assert false
      | PacketIn _
      | HostUp _
      | HostDown _
      | Query _ ->
        return [e]

  let create () =
    let open Async_NetKAT in
    let open NetKAT_Types in
    let flip_ctl, flip_app =
      Flip.create (And(Test(EthType Probe.protocol), (Test(EthSrc Probe.mac))))
    in
    let r_pkt_outs, pkt_outs = Pipe.create () in
    let edge_app = Raw.create [] (fun nib send () ->
      Deferred.don't_wait_for (Pipe.transfer_id r_pkt_outs send);
      function
      | PacketIn _
      | Query _ -> return None
      | _       -> return (Some (TUtil.edge !nib)))
    in
    let probe_period = Time.Span.of_sec 3.0 in
    let t = { flip_ctl; flip_app; edge_app; pkt_outs; probe_period; loop = return () } in
    let app =
      ap ~how:`Parallel
        (lift (fun _ a -> a) edge_app)
        (guard' flip_app (Policy.create_static (Mod(Location(Pipe "probe")))))
    in
    t, stage t, app

  let managed_traffic t =
    t.flip_app

  let set_link_probe_period t probe_period =
    t.probe_period <- probe_period

  let get_link_probe_period t =
    t.probe_period

  let start t =
    Flip.enable t.flip_ctl
    >>| fun () -> t.loop <- loop t

  let stop t =
    Deferred.all_ignore [ Flip.disable t.flip_ctl; t.loop ]
    >>| fun () -> t.loop <- return ()

end

module Host = struct
  module Log = Async_OpenFlow.Log
  let tags = [("netkat", "topology.host")]

  type t = Flip.t

  let enabled t =
    t.Flip.enabled

  let stage t = fun nib e ->
    let open Net.Topology in
    let open NetKAT_Types in
    let open Async_NetKAT in
    match e with
    | PacketIn ("host", sw_id, pt_id, payload, len) when enabled t ->
      let open Packet in
      let dlAddr, nwAddr = match parse (SDN_Types.payload_bytes payload) with
        | { nw = Arp (Arp.Query(dlSrc, nwSrc, _ )) }
        | { nw = Arp (Arp.Reply(dlSrc, nwSrc, _, _)) } ->
          (dlSrc, nwSrc)
        | _ -> assert false in
      let h = try Some(vertex_of_label !nib (Host(dlAddr, nwAddr)))
        with _ ->  None in
      begin match TUtil.in_edge !nib sw_id pt_id, h with
        | true, None    ->
          let nib', h = add_vertex !nib (Host(dlAddr, nwAddr)) in
          let nib', s = add_vertex nib' (Switch sw_id) in
          let nib', _ = add_edge nib' s pt_id () h 0l in
          let nib', _ = add_edge nib' h 0l () s pt_id in
          nib := nib';
          Log.info ~tags "[topology.host] ↑ { switch = %Lu; port %lu }, { ip = %s; mac = %s }"
            sw_id pt_id
            (Packet.string_of_ip nwAddr)
            (Packet.string_of_mac dlAddr);
          return [e; HostUp((sw_id, pt_id), (dlAddr, nwAddr))]
        | _   , _       -> return [e]
      end
    | PortDown (sw_id, pt_id) ->
      let v = vertex_of_label !nib (Switch sw_id) in
      let mh = next_hop !nib v pt_id in
      (* Do not put the above line below the below line. The code needs to
       * read the current view of the network before modifying it. *)
      begin match mh with
        | None -> return [e]
        | Some(edge) ->
          let (v2, pt_id2) = edge_dst edge in
          begin match vertex_to_label !nib v2 with
            | Switch _ -> return [e]
            | Host (dlAddr, nwAddr) ->
              nib := remove_endpoint !nib (v, pt_id);
              Log.info ~tags "[topology.host] ↓ { switch = %Lu; port %lu }, { ip = %s; mac = %s }"
                sw_id pt_id
                (Packet.string_of_ip nwAddr)
                (Packet.string_of_mac dlAddr);
              return [e; HostDown((sw_id, pt_id), (dlAddr, nwAddr))]
          end
      end
    | HostUp _ ->
      assert false
    | HostDown _ ->
      assert false
    | _ -> return [e]

  let create () =
    let open NetKAT_Types in
    let open Async_NetKAT in
    let t, flip = Flip.create (Test(EthType 0x0806)) in
    let to_host = Policy.create_static (Mod(Location(Pipe "host"))) in
    t, stage t, guard' flip to_host

  let start t =
    Flip.enable t

  let stop t =
    Flip.disable t

end

type s = (Net.Topology.t ref, NetKAT_Types.event, NetKAT_Types.event) Async_OpenFlow.Stage.t
type t =
  { (* *)
    host_ctl : Flip.t;
    (* *)
    switch_ctl : Switch.t;
}

let create () =
  let switch_ctl, switch_stage, switch = Switch.create () in
  let host_ctl, host_stage, host = Host.create () in
  let t = { host_ctl; switch_ctl } in
  let stage =
    Async_OpenFlow.Stage.(host_stage >=> switch_stage)
  in
  (t, stage, Async_NetKAT.union host switch)

let managed_traffic t =
  Switch.managed_traffic t.switch_ctl

let start t =
  Deferred.all_unit [Switch.start t.switch_ctl; Host.start t.host_ctl]

let stop t =
  Deferred.all_unit [Switch.stop t.switch_ctl; Host.stop t.host_ctl]
