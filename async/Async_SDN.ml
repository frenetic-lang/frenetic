open Core.Std
open Async.Std

module Chunk_Controller = Async_OpenFlowChunk.Controller
module OF0x01_Controller = Async_OpenFlow0x01.Controller
module OF0x04_Controller = Async_OpenFlow0x04.Controller

module SDN = SDN_Types

module ClientMap = Chunk_Controller.Client_id.Table

module Conn = struct
  type t = { version : int }
end

type t =
  { sub_chunk : Chunk_Controller.t
  ; sub_0x01 : OF0x01_Controller.t
  ; sub_0x04 : OF0x04_Controller.t
  ; conns : Conn.t ClientMap.t
  }

type e = [
  | `Connect    of SDN.switchId * SDN.switchFeatures
  | `Disconnect of SDN.switchId * Sexp.t
  | `PacketIn   of SDN.switchId * SDN.pktIn
  | `PortUp     of SDN.switchId * SDN.portId
  | `PortDown   of SDN.switchId * SDN.portId ]


let set_monitor_interval (t:t) (s:Time.Span.t) : unit =
  Chunk_Controller.set_monitor_interval t.sub_chunk s

let set_idle_wait (t:t) (s:Time.Span.t) : unit =
  Chunk_Controller.set_idle_wait t.sub_chunk s

let set_kill_wait (t:t) (s:Time.Span.t) : unit =
  Chunk_Controller.set_kill_wait t.sub_chunk s


let create ?max_pending_connections
    ?verbose
    ?log_disconnects
    ?buffer_age_limit
    ?monitor_connections ~port () =
  Chunk_Controller.create ?max_pending_connections ?verbose ?log_disconnects
    ?buffer_age_limit ?monitor_connections ~port ()
  >>| function chunk ->
      { sub_chunk = chunk
      ; sub_0x01 = OF0x01_Controller.create_from_chunk chunk
      ; sub_0x04 = OF0x04_Controller.create_from_chunk chunk
      ; conns = ClientMap.create ()
      }

let listen_of0x01 (t : t) : Chunk_Controller.h Pipe.Writer.t * e Pipe.Reader.t =
  let module OF = OpenFlow0x01 in
  let recv, send = Pipe.create () in
  let port_useable (pd : OF.portDescription) =
    if pd.config.down
      then false
      else not (pd.state.down)
  in
  let get_port pd =
    if port_useable pd
      then Some(Int32.of_int_exn pd.port_no)
      else None
  in
  send, Pipe.filter_map (OF0x01_Controller.listen_pipe t.sub_0x01 recv)
    ~f:(function
        | `Connect(sw_id, fs) ->
          let sdn_fs =
            { SDN.switch_id = sw_id
            ; SDN.switch_ports = List.filter_map fs.OF.SwitchFeatures.ports
                ~f:get_port
            }
          in
          Some(`Connect(sw_id, sdn_fs))
        | `Disconnect(sw_id, exn) ->
          Some(`Disconnect(sw_id, exn))
        | `Message(sw_id, (xid, OF.Message.PacketInMsg pi)) ->
          Some(`PacketIn(sw_id, SDN_OpenFlow0x01.to_packetIn pi))
        | `Message (sw_id, (xid, OF.Message.PortStatusMsg ps)) ->
          let open OF.PortStatus in
          let open OF.PortDescription in
          begin match ps.reason, port_useable ps.desc with
          | ChangeReason.Add, true
          | ChangeReason.Modify, true ->
            let pt_id = Int32.of_int_exn (ps.desc.port_no) in
            Some(`PortUp(sw_id, pt_id))
          | ChangeReason.Delete, _
          | ChangeReason.Modify, false ->
            let pt_id = Int32.of_int_exn (ps.desc.port_no) in
            Some(`PortDown(sw_id, pt_id))
          | _ ->
            None
          end
        | `Message _ -> failwith "NYI.listen_of0x01.Message")

let listen_of0x04 (t : t) : Chunk_Controller.h Pipe.Writer.t * e Pipe.Reader.t =
  let module OF = OpenFlow0x04 in
  let open OpenFlow0x04_Core in
  let recv, send = Pipe.create () in
  let port_useable (pd : portDesc) =
    if pd.config.port_down
      then false
      else not (pd.state.link_down)
  in
  let get_port pd =
    if port_useable pd
      then Some(pd.port_no)
      else None
  in
  send, Pipe.filter_map (OF0x04_Controller.listen_pipe t.sub_0x04 recv)
    ~f:(function
        | `Connect(sw_id, (fs, ps)) ->
          let sdn_fs =
            { SDN.switch_id = sw_id
            ; SDN.switch_ports = List.filter_map ps ~f:get_port }
          in
          Some(`Connect(sw_id, sdn_fs))
        | `Disconnect(sw_id, exn) ->
          Some(`Disconnect(sw_id, exn))
        | `Message(sw_id, (xid, OF.Message.PacketInMsg pi)) ->
          Some(`PacketIn(sw_id, SDN_OpenFlow0x04.to_packetIn pi))
        | `Message (sw_id, (xid, OF.Message.PortStatusMsg ps)) ->
          begin match ps.reason, port_useable ps.desc with
          | PortAdd, true
          | PortModify, true ->
            Some(`PortUp(sw_id, ps.desc.port_no))
          | PortDelete, _
          | PortModify, false ->
            Some(`PortDown(sw_id, ps.desc.port_no))
          | _ ->
            None
          end
        | `Message _ ->
          failwith "NYI.listen_of0x04.Message")

let listen (t : t) : e Pipe.Reader.t =
  let of0x01_w, of0x01_r = listen_of0x01 t in
  let of0x04_w, of0x04_r = listen_of0x04 t in
  let events =
    let open Async_OpenFlow_Stage in
    let open Chunk_Controller in
    let stages = echo >=> txn >=> handshake 0x04 in
    run stages t.sub_chunk (listen t.sub_chunk)
  in
  Deferred.don't_wait_for (Pipe.iter events ~f:(fun e ->
    let version = match e with
    | `Connect(c_id, version) ->
      begin match ClientMap.add t.conns c_id { Conn.version } with
      | `Duplicate -> assert false
      | `Ok -> version
      end
    | `Disconnect(c_id, _) ->
      begin match ClientMap.find_and_remove t.conns c_id with
      | None -> assert false
      | Some(conn) -> conn.Conn.version
      end;
    | `Message(c_id, _) ->
      begin match ClientMap.find t.conns c_id with
      | None -> assert false
      | Some(conn) -> conn.Conn.version
      end
    in
    match version with
    | 0x01 -> Pipe.write of0x01_w e
    | 0x04 -> Pipe.write of0x04_w e
    | v -> failwith Printf.(sprintf "SDN.listen: unsupported version: %d" v)));
  Pipe.interleave [of0x01_r; of0x04_r]

let switch_version (t : t) (sw_id : SDN.switchId) =
  match OF0x01_Controller.client_id_of_switch t.sub_0x01 sw_id with
  | None ->
    begin match OF0x04_Controller.client_id_of_switch t.sub_0x04 sw_id with
    | None -> raise Not_found
    | Some _ -> 0x04
    end
  | Some _ -> 0x01

let clear_flows ?(pattern:SDN.Pattern.t option) (t:t) (sw_id:SDN.switchId) =
  match switch_version t sw_id with
  | 0x01 ->
    let pattern = Option.map pattern ~f:SDN_OpenFlow0x01.from_pattern in
    OF0x01_Controller.clear_flows ?pattern t.sub_0x01 sw_id
  | 0x04 ->
    let pattern = match Option.map pattern ~f:SDN_OpenFlow0x04.from_pattern with
    | Some(pattern, _) -> Some(pattern)
    | None             -> None
    in
    OF0x04_Controller.clear_flows ?pattern t.sub_0x04 sw_id
  | v -> failwith Printf.(sprintf "SDN.clear_flows: unsupported version: %d" v)

let install_flows ?(clear=true) (t : t) (sw_id : SDN.switchId) flows =
  let priority = ref 65536 in
  match switch_version t sw_id with
  | 0x01 ->
    let f flow = decr priority; SDN_OpenFlow0x01.from_flow !priority flow in
    let flow_mods = List.map flows ~f in
    OF0x01_Controller.send_flow_mods ~clear t.sub_0x01 sw_id flow_mods
  | 0x04 ->
    let groups = GroupTable0x04.create () in
    let f flow = decr priority; SDN_OpenFlow0x04.from_flow groups !priority flow in
    let flow_mods = List.map flows ~f in
    let open Deferred.Result in
    OF0x04_Controller.clear_groups t.sub_0x04 sw_id >>= fun () ->
    OF0x04_Controller.send_flow_mods ~clear t.sub_0x04 sw_id flow_mods >>= fun () ->
    let f group = OF0x04_Controller.send_result t.sub_0x04 sw_id (0l, group) in
    Deferred.Result.all_ignore (List.map (GroupTable0x04.commit groups) ~f)
  | v -> failwith Printf.(sprintf "SDN.install_flows: unsupported version: %d" v)

let send_pkt_out (t : t) (sw_id : SDN.switchId) (pkt_out : SDN.pktOut) =
  match switch_version t sw_id with
  | 0x01 -> OF0x01_Controller.send_pkt_out t.sub_0x01 sw_id
      (SDN_OpenFlow0x01.from_packetOut pkt_out)
  | 0x04 -> OF0x04_Controller.send_pkt_out t.sub_0x04 sw_id
      (SDN_OpenFlow0x04.from_packetOut pkt_out)
  | v -> failwith Printf.(sprintf "SDN.send_pkt_out: unsupported version: %d" v)

let barrier (t : t) (sw_id : SDN.switchId) =
  match switch_version t sw_id with
  | 0x01 -> OF0x01_Controller.barrier t.sub_0x01 sw_id
  | 0x04 -> OF0x04_Controller.barrier t.sub_0x04 sw_id
  | v -> failwith Printf.(sprintf "SDN.barrier: unsupported version: %d" v)
