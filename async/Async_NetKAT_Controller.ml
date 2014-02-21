open Core.Std
open Async.Std

open Topology

module Controller = Async_OpenFlow.OpenFlow0x01.Controller
module SDN = SDN_Types
module NetKAT = NetKAT_Types

module SwitchMap = Map.Make(Int64)

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

type t = {
  ctl : Controller.t;
  mutable locals : LocalCompiler.t SwitchMap.t
}

let bytes_to_headers port_id (bytes : Cstruct.t) =
  let open NetKAT.Headers in
  let open Packet in
  let pkt = Packet.parse bytes in
  { location = Some(NetKAT.Physical port_id)
  ; ethSrc = Some(pkt.dlSrc)
  ; ethDst = Some(pkt.dlDst)
  ; vlan = pkt.dlVlan
  ; vlanPcp = Some(pkt.dlVlanPcp)
  ; ethType = Some(dlTyp pkt)
  ; ipProto = (try Some(nwProto pkt) with Invalid_argument(_) -> None)
  ; ipSrc = (try Some((nwSrc pkt, 0)) with Invalid_argument(_) -> None)
  ; ipDst = (try Some((nwDst pkt, 0)) with Invalid_argument(_) -> None)
  ; tcpSrcPort = (try Some(tpSrc pkt) with Invalid_argument(_) -> None)
  ; tcpDstPort = (try Some(tpDst pkt) with Invalid_argument(_) -> None)
  }

let headers_to_actions port (h:NetKAT_Types.Headers.t) : SDN_Types.action list =
  let open SDN_Types in
  let g p acc f =
    match Field.get f h with
      | Some v -> (p v)::acc
      | None -> acc in
  let init = match h.NetKAT_Types.Headers.location with
     | Some(NetKAT_Types.Pipe     _) -> assert false
     | Some(NetKAT_Types.Physical p) -> [OutputPort(VInt.Int32 p)]
     | None -> [OutputPort (VInt.Int32 (Int32.of_int_exn port))] in
  NetKAT_Types.Headers.Fields.fold
    ~init
    ~location:(fun acc f -> acc)
    ~ethSrc:(g (fun v -> SetField(EthSrc, VInt.Int48 v)))
    ~ethDst:(g (fun v -> SetField(EthDst, VInt.Int48 v)))
    ~vlan:(g (fun v -> SetField(Vlan, VInt.Int16 v)))
    ~vlanPcp:(g (fun v -> SetField(VlanPcp, VInt.Int8 v)))
    ~ethType:(g (fun v -> SetField(EthType, VInt.Int16 v)))
    ~ipProto:(g (fun v -> SetField(IPProto, VInt.Int8 v)))
    ~ipSrc:(g (fun (v, _) -> SetField(IP4Src, VInt.Int32 v)))
    ~ipDst:(g (fun (v, _) -> SetField(IP4Dst, VInt.Int32 v)))
    ~tcpSrcPort:(g (fun v -> SetField(TCPSrcPort, VInt.Int16 v)))
    ~tcpDstPort:(g (fun v -> SetField(TCPDstPort, VInt.Int16 v)))

exception Unsupported_mod of string

let payload_bytes p = match p with
  | SDN_Types.NotBuffered(bytes)
  | SDN_Types.Buffered(_, bytes) -> bytes

let packet_sync_headers (pkt:NetKAT_Types.packet) : NetKAT_Types.packet * bool =
  let open NetKAT_Types in
  let change = ref false in
  let g p q acc f =
    match Field.get f pkt.headers with
      | Some v -> if p v acc then
          acc
        else begin
          change := true;
          q acc v
        end
      | None -> acc in
  let fail field = (fun _ -> raise (Unsupported_mod field)) in
  let packet = Packet.parse (payload_bytes pkt.payload) in
  let packet' = Headers.Fields.fold
    ~init:packet
    ~location:(fun acc _ -> acc)
    ~ethSrc:(g (fun v p -> v = p.Packet.dlSrc) Packet.setDlSrc)
    ~ethDst:(g (fun v  p -> v = p.Packet.dlDst) Packet.setDlDst)
    (* XXX(seliopou): Fix impls of: vlan, vlanPcp *)
    ~vlan:(g (fail "vlan") (fail "vlan"))
    ~vlanPcp:(g (fun _ _ -> true) (fail "vlanPcp"))
    ~ipSrc:(g (fun (v, _) p -> v = Packet.nwSrc p) (fun acc (nw, _) -> Packet.setNwSrc acc nw))
    ~ipDst:(g (fun (v, _) p -> v = Packet.nwDst p) (fun acc (nw, _) -> Packet.setNwDst acc nw))
    ~tcpSrcPort:(g (fun v p -> v = Packet.tpSrc p) Packet.setTpSrc)
    ~tcpDstPort:(g (fun v p -> v = Packet.tpDst p) Packet.setTpDst)
    (* XXX(seliopou): currently does not support: *)
    ~ethType:(g (fun _ _ -> true) (fail "ethType"))
    ~ipProto:(g (fun _ _ -> true) (fail "ipProto")) in
  ({ pkt with payload = match pkt.payload with
    | SDN_Types.NotBuffered(_) -> SDN_Types.NotBuffered(Packet.marshal packet')
    | SDN_Types.Buffered(n, _) -> SDN_Types.Buffered(n, Packet.marshal packet')
  }, !change)

let packet_out_to_message mpt (_, bytes, buffer_id, actions) =
  let open OpenFlow0x01_Core in
  let output_payload = match buffer_id with
    | Some(id) -> Buffered(id, bytes)
    | None -> NotBuffered bytes in
  let apply_actions = SDN_OpenFlow0x01.from_group mpt [[actions]] in
  OpenFlow0x01.Message.PacketOutMsg{ output_payload; port_id = None; apply_actions }

let send t c_id msg =
  Controller.send t c_id msg
  >>| function
    | `Sent _ -> ()
    | `Drop exn -> raise exn

let to_event (t : t) evt =
  let open NetKAT_Types in
  match evt with
    | `Connect (c_id, feats) ->
      return [SwitchUp feats.OpenFlow0x01.SwitchFeatures.switch_id]
    | `Disconnect (c_id, switch_id, exn) ->
      return [SwitchDown switch_id]
    | `Message (c_id, (xid, msg)) ->
      let open OpenFlow0x01.Message in
      begin match msg with
        | PacketInMsg pi ->
          let open OpenFlow0x01_Core in
          let switch_id = Controller.switch_id_of_client t.ctl c_id in
          begin match SwitchMap.find t.locals switch_id with
            | None ->
              (* The switch may be connected but has yet had rules installed on
               * it. In that case, just drop the packet.
               * *)
              return []
            | Some local ->
              (* Eval the packet to get the list of packets that should go to
               * pipes, and the list of packets that can be forwarded to physical
               * locations.
               * *)
              let buf_id, bytes = match pi.input_payload with
                | Buffered(n, bs) -> Some(n), bs
                | NotBuffered(bs) -> None, bs in
              let packet = {
                switch = switch_id;
                headers = bytes_to_headers (Int32.of_int_exn pi.port) bytes;
                payload = SDN_OpenFlow0x01.to_payload pi.input_payload
              } in
              begin
                let pis, phys = LocalCompiler.eval local packet in
                let outs = Deferred.List.map phys ~f:(fun packet1 ->
                  let acts = headers_to_actions pi.port
                    (Headers.diff packet1.headers packet.headers) in
                  let po = (switch_id, bytes, buf_id, acts) in
                  send t.ctl c_id (0l, packet_out_to_message (Some pi.port) po)) in
                Deferred.ignore outs >>= fun _ ->
                return (List.map pis ~f:(fun (p, pkt) ->
                  let port_id = VInt.Int16 pi.port in
                  let pkt', changed = packet_sync_headers pkt in
                  if changed then
                    let bytes = payload_bytes pkt'.payload in
                    PacketIn(p, switch_id, port_id, bytes, pi.total_len, None)
                  else
                    PacketIn(p, switch_id, port_id, bytes, pi.total_len, buf_id)))
              end
          end
        | _ ->
          let sw_id = Controller.switch_id_of_client t.ctl c_id in
          Log.debug ~tags "Dropped message from %Lu: %s" sw_id (to_string msg);
          return []
      end

let update_table_for (t : t) (sw_id : switchId) pol =
  let delete_flows =
    OpenFlow0x01.Message.FlowModMsg OpenFlow0x01_Core.delete_all_flows in
  let to_flow_mod prio flow =
    OpenFlow0x01.Message.FlowModMsg (SDN_OpenFlow0x01.from_flow prio flow) in
  let c_id = Controller.client_id_of_switch t.ctl sw_id in
  let local = LocalCompiler.of_policy sw_id pol in
  t.locals <- SwitchMap.add t.locals sw_id local;
  send t.ctl c_id (0l, delete_flows) >>= fun _ ->
  let priority = ref 65536 in
  let table = LocalCompiler.to_table local in
  Deferred.ignore (Deferred.List.map table ~f:(fun flow ->
    decr priority;
    send t.ctl c_id (0l, to_flow_mod !priority flow)))

let handler (t : t) app =
  let app' = Async_NetKAT.run app in
  fun e ->
    let open Deferred in
    let nib = Controller.nib t.ctl in
    app' nib e >>= fun (packet_outs, m_pol) ->
    let outs = List.map packet_outs ~f:(fun ((sw_id,_,_,_) as po) ->
      (* XXX(seliopou): xid *)
      let c_id = Controller.client_id_of_switch t.ctl sw_id in
      send t.ctl c_id (0l, packet_out_to_message None po)) in
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
  let stages = let open Controller in
    (local (fun t -> t.ctl)
      (features >=> topology))
     >=> to_event in

  Controller.create ~max_pending_connections ~port ()
  >>> fun t ->
    let t' = { ctl = t; locals = SwitchMap.empty } in
    let events = run stages t' (Controller.listen t) in
    Deferred.don't_wait_for (Pipe.iter events ~f:(handler t' app))
