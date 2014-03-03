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

  module Ctl = struct
    module PortSet = Async_NetKAT.Net.Topology.PortSet

    type t = {
      r_evts : NetKAT_Types.event Pipe.Reader.t;
      (* NB: Do not write with pushback to the event pipe. These events will
       * evetually be fed back into this app, so blocking on the write may cause
       * a deadlock.
       * *)
      w_evts : NetKAT_Types.event Pipe.Writer.t;
      w_outs : NetKAT_Types.packet_out Pipe.Writer.t;
      mutable pending : PortSet.t SwitchMap.t;
      mutable probe : bool;
      mutable loop : unit Deferred.t
    }

    let to_out switch_id port_id =
      let bytes = Packet.marshal
        Probe.(to_packet { switch_id; port_id = port_id }) in
      let port_id' = VInt.Int32 port_id in
      let action = SDN_Types.OutputPort(port_id') in
      (switch_id, bytes, None, Some(port_id'), [action])

    let loop t =
      Deferred.repeat_until_finished () (fun () ->
        if t.probe then
          Clock.after (Time.Span.of_sec 3.0)
          >>= fun () ->
          let outs = SwitchMap.fold t.pending ~init:[] ~f:(fun ~key ~data acc ->
            PortSet.fold (fun pt_id acc -> (to_out key pt_id):: acc) data acc) in
          Deferred.List.iter outs ~f:(Pipe.write t.w_outs)
          >>| fun _ -> `Repeat ()
        else
          return (`Finished ()))

    let send_event t evt =
      (* NB: See the deadlock comment above. *)
      Pipe.write_without_pushback t.w_evts evt

    let events t =
      t.r_evts

    let add t switch_id port_id =
      t.pending <- SwitchMap.change t.pending switch_id (function
        | None -> Some(PortSet.singleton port_id)
        | Some(ports) -> Some(PortSet.add port_id ports));
      Pipe.write t.w_outs (to_out switch_id port_id)

    let remove t switch_id port_id =
      let removed = ref true in
      t.pending <- SwitchMap.change t.pending switch_id (function
        | None -> removed := false; None
        | Some(ports) -> Some(PortSet.remove port_id ports));
      !removed

    let remove_switch t switch_id =
      t.pending <- SwitchMap.remove t.pending switch_id

    let is_pending t switch_id port_id =
      match SwitchMap.find t.pending switch_id with
        | None -> false
        | Some(ps) -> PortSet.mem port_id ps

    let pause t =
      t.probe <- false;
      t.loop
      >>| fun () ->
        t.loop <- return ()

    let resume t =
      t.probe <- true;
      t.loop <- loop t

    let create w_outs =
      let r_evts, w_evts = Pipe.create () in
      let t = {
        r_evts;
        w_evts;
        w_outs;
        pending = SwitchMap.empty;
        probe = false;
        loop = return ()
      } in
      resume t;
      t
  end

  let guard app =
    let open NetKAT_Types in
    let open Async_NetKAT in
    let gpol = Filter(And(Neg(Test(EthSrc Probe.mac)),
                          Neg(Test(EthType Probe.protocol)))) in
    seq (create_static gpol) app

  let create () =
    let open NetKAT_Types in
    let open Async_NetKAT in
    (* The default and static policy. This should be installed on all switches
     * and never change. *)
    let default = Seq(Filter(And(Test(EthType Probe.protocol),
                                 Test(EthSrc  Probe.mac))),
                      Mod(Location(Pipe("probe")))) in
    (* A pipe for packet_outs *)
    let o_r, o_w = Pipe.create () in

    (* Stores state for switch topology discovery, and manages the logical
     * thread that sends probes to unknown ports *)
    let ctl = Ctl.create o_w in

    let handle_probe t switch_id port_id probe : Net.Topology.t =
      let open Probe in
      let open Net.Topology in
      begin if not (Ctl.remove ctl probe.switch_id probe.port_id) then
        raise (Assertion_failed (Printf.sprintf
            "Discovery.handle_probe: unknown switch %Lu" probe.switch_id));
      end;
      (* It is possible for the controller to receive PacketIn events from a
       * switch before its probes are sent. It however should not be possible to
       * get a PacketIn after a switch has disconnect. *)
      ignore (Ctl.remove ctl switch_id port_id);
      let v = vertex_of_label t (Switch switch_id) in
      match next_hop t v port_id with
        | Some(e) ->
          let v, port_id' = edge_dst e in
          begin if not (probe.port_id = port_id') then
            raise (Assertion_failed (Printf.sprintf
                    "Discovery.handle_probe: probe inconsistent w/ topology"))
          end;
          t
        | None ->
          let t, v1 = add_vertex t (Switch switch_id) in
          let t, v2 = add_vertex t (Switch probe.switch_id) in
          let t, _  = add_edge t v1 port_id () v2 probe.port_id in
          let t, _  = add_edge t v2 probe.port_id () v1 port_id in
          let e1 = (switch_id, VInt.Int32 port_id) in
          let e2 = (probe.switch_id, VInt.Int32 probe.port_id) in
          Ctl.send_event ctl (LinkUp (e1, e2));
          t in

    let handler t w () : event -> result Deferred.t = fun e ->
      (* Transfer all packet_outs from the Clt.t to the pipe passed to the
       * handler. *)
      Deferred.don't_wait_for (Pipe.transfer_id o_r w);
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
              t := handle_probe !t sw_id pt_id' (Probe.parse bytes)
            | _ -> ();
          end;
          return None
        | SwitchUp sw_id ->
          Log.info ~tags "[topology.switch] ↑ { switch = %Lu }" sw_id;
          t := fst (add_vertex !t (Switch sw_id));
          return None
        | SwitchDown sw_id ->
          Log.info ~tags "[topology.switch] ↓ { switch = %Lu }" sw_id;
          Ctl.remove_switch ctl sw_id;
          t := remove_vertex !t (vertex_of_label !t (Switch sw_id));
          return None
        | PortUp (sw_id, pt_id) ->
          let pt_id' = VInt.get_int32 pt_id in
          Log.info ~tags "[topology.switch] ↑ { switch = %Lu; port = %lu }" sw_id pt_id';
          Ctl.add ctl sw_id pt_id'
          >>| fun () ->
            None
        | PortDown (sw_id, pt_id) ->
          let pt_id' = VInt.get_int32 pt_id in
          Log.info ~tags "[topology.switch] ↓ { switch = %Lu; port = %lu }" sw_id pt_id';
          ignore (Ctl.remove ctl sw_id pt_id');
          let v = vertex_of_label !t (Switch sw_id) in
          let mh = next_hop !t v pt_id' in
          begin match mh with
            | None -> ()
            | Some(e) ->
              let (v2, pt_id2) = edge_dst e in
              begin match vertex_to_label !t v2 with
                | Switch(sw_id2) ->
                  t := remove_endpoint !t (v, pt_id');
                  Ctl.send_event ctl
                    (LinkDown((sw_id, pt_id), (sw_id2, VInt.Int32 pt_id2)))
                | Host _ -> ()
              end
          end;
          return None
        | LinkUp ((sw1, pt1), (sw2, pt2)) ->
          Log.info ~tags "[topology.switch] ↑ { switch = %Lu; port %lu }, { switch = %Lu; port = %lu }"
            sw1 (VInt.get_int32 pt1)
            sw2 (VInt.get_int32 pt2);
          return None
        | LinkDown ((sw1, pt1), (sw2, pt2)) ->
          Log.info ~tags "[topology.switch] ↓ { switch = %Lu; port %lu }, { switch = %Lu; port = %lu }"
            sw1 (VInt.get_int32 pt1)
            sw2 (VInt.get_int32 pt2);
          return None
        | HostUp _
        | HostDown _
        | Query _ ->
          return None in

    let app = create ~pipes:(PipeSet.singleton "probe") default handler in
    (ctl, app)
end

module Host = struct
  module Log = Async_OpenFlow.Log
  let tags = [("netkat", "topology.host")]

  let create ctl () =
    let open NetKAT_Types in
    let open Async_NetKAT in
    let default = Seq(Filter(Test(EthType 0x0806)),
                      Mod(Location(Pipe("host")))) in

    let handler t w () : event -> result Deferred.t = fun e ->
      let open Net.Topology in
      match e with
        | PacketIn (_, sw_id, pt_id, bytes, len, buf) ->
          let pt_id' = VInt.get_int32 pt_id in
          let open Packet in
          let dlAddr, nwAddr = match parse bytes with
            | { nw = Arp (Arp.Query(dlSrc, nwSrc, _ )) }
            | { nw = Arp (Arp.Reply(dlSrc, nwSrc, _, _)) } ->
              (dlSrc, nwSrc)
            | _ -> assert false in
          let h = try Some(vertex_of_label !t (Host(dlAddr, nwAddr)))
            with _ ->  None in
          begin match Switch.Ctl.is_pending ctl sw_id pt_id', h with
            | true, None    ->
              let t', h = add_vertex !t (Host(dlAddr, nwAddr)) in
              let t', s = add_vertex t' (Switch sw_id) in
              let t', _ = add_edge t' s pt_id' () h 0l in
              let t', _ = add_edge t' h 0l () s pt_id' in
              t := t';
              Switch.Ctl.send_event ctl (HostUp((sw_id, pt_id), (dlAddr, nwAddr)));
              return None
            | _   , _       -> return None
          end
        | PortDown (sw_id, pt_id) ->
          let pt_id' = VInt.get_int32 pt_id in
          ignore (Switch.Ctl.remove ctl sw_id pt_id');
          let v = vertex_of_label !t (Switch sw_id) in
          let mh = next_hop !t v pt_id' in
          (* Do not put the above line below the below line. The code needs to
           * read the current view of the network before modifying it. *)
          begin match mh with
            | None -> ()
            | Some(e) ->
              let (v2, pt_id2) = edge_dst e in
              begin match vertex_to_label !t v2 with
                | Switch _ -> ()
                | Host (dlAddr, nwAddr) ->
                  t := remove_endpoint !t (v, pt_id');
                  Switch.Ctl.send_event ctl
                    (HostDown((sw_id, pt_id), (dlAddr, nwAddr)))
              end
          end;
          return None
        | HostUp ((sw_id, pt_id), (dlAddr, nwAddr)) ->
          Log.info ~tags "[topology.host] ↑ { switch = %Lu; port %lu }, { ip = %s; mac = %s }"
            sw_id (VInt.get_int32 pt_id)
            (Packet.string_of_ip nwAddr)
            (Packet.string_of_mac dlAddr);
          return None
        | HostDown ((sw_id, pt_id), (dlAddr, nwAddr)) ->
          Log.info ~tags "[topology.host] ↓ { switch = %Lu; port %lu }, { ip = %s; mac = %s }"
            sw_id (VInt.get_int32 pt_id)
            (Packet.string_of_ip nwAddr)
            (Packet.string_of_mac dlAddr);
          return None
        | SwitchUp _
        | SwitchDown _
        | PortUp _
        | LinkUp _
        | LinkDown _
        | Query _ ->
          return None in

    create ~pipes:(PipeSet.singleton "host") default handler
end

let guard app =
  Switch.guard app

let events t =
  Switch.Ctl.events t

let create () =
  let ctl, switch = Switch.create () in
  let host = Host.create ctl () in
  (ctl, Async_NetKAT.union host switch)

let pause t =
  Switch.Ctl.pause t

let resume t =
  Switch.Ctl.resume t
