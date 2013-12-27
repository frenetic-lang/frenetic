open Async.Std
open Core.Std

open OpenFlow0x01
open OpenFlow0x01_Core
open OpenFlow0x01.Message

module OF0x01Controller = Async_OpenFlow0x01.Controller

module SwitchTable = OF0x01Controller.SwitchTable

module EthTable = Map.Make(struct
  type t = Int64.t sexp_opaque with sexp
  let compare = Pervasives.compare
end)

type switchTable = portId EthTable.t SwitchTable.t

let switch
  (ctl : OF0x01Controller.t)
  (tbl : switchTable)
   evt =
  let ensure result response =
    begin match response with
      | `Sent _   -> return result
      | `Drop exn -> raise exn
    end in

  let learn c_id pi =
    let pkt = parse_payload pi.input_payload in
    let eth, port = pkt.Packet.dlSrc, pi.port in
    let tbl', present = match SwitchTable.find tbl c_id with
      | None -> (SwitchTable.add tbl c_id (EthTable.singleton eth port), false)
      | Some e_tbl ->
        match EthTable.find e_tbl eth with
          | None -> (SwitchTable.add tbl c_id (EthTable.add e_tbl eth port), false)
          | Some port' when port = port' -> (tbl, true)
          | _ -> failwith "Inconsistent topology" in (* XXX(seliopou): exn *)
    if present then
      return (true, tbl')
    else
      let fwd, buf = match pi.input_payload with
        | Buffered (b_id, _) -> (false, Some b_id)
        | NotBuffered _ -> (true, None) in
      OF0x01Controller.send ctl c_id
        (1l, FlowModMsg {
          command = AddFlow;
          pattern = { match_all with dlDst = Some(eth) };
          priority = 1;
          actions = [ Output(PhysicalPort port) ];
          cookie = 0L;
          idle_timeout = Permanent;
          hard_timeout = Permanent;
          notify_when_removed = false;
          apply_to_packet = buf;
          out_port = None;
          check_overlap = false
        })
        (* XXX(seliopu): can ensure asynchronously if not buffered, or if the
         * buffering is ignored completely.
         * *)
        >>= ensure (fwd, tbl') in

  let forward (tbl : switchTable) c_id t_id pi =
    let dst = (parse_payload pi.input_payload).Packet.dlDst in
    let out =
      match EthTable.find (SwitchTable.find_exn tbl c_id) dst with
        | None -> Flood
        | Some(p) -> PhysicalPort(p) in
    OF0x01Controller.send ctl c_id
      (t_id, PacketOutMsg {
          output_payload = pi.input_payload;
          port_id = Some(pi.port);
          apply_actions = [ Output(out) ] }) in

  begin match evt with
    | `Connect c_id ->
      OF0x01Controller.send ctl c_id
        (0l, FlowModMsg {
          command = AddFlow;
          pattern = match_all;
          priority = 1;
          actions = [ Output(Controller 1024) ];
          cookie = 0L;
          idle_timeout = Permanent;
          hard_timeout = Permanent;
          notify_when_removed = false;
          apply_to_packet = None;
          out_port = None;
          check_overlap = false
        })
      >>= ensure (SwitchTable.add tbl c_id EthTable.empty)
    | `Disconnect (c_id, _) ->
      return (SwitchTable.remove tbl c_id)
    | `Message (c_id, msg) ->
      let t_id, msg = msg in
      begin match msg with
        | EchoRequest bytes ->
          OF0x01Controller.send ctl c_id (t_id, EchoReply bytes) >>= ensure tbl
        | PacketInMsg pi ->
          learn c_id pi
            >>= (function
              | (true , tbl') -> forward tbl' c_id t_id pi >>= ensure tbl'
              | (false, tbl') -> return tbl')
        | ErrorMsg err -> failwith (Error.to_string err)
        | PortStatusMsg ps ->
          let open PortStatus in
          let port = ps.desc.PortDescription.port_no in
          begin match ps.reason with
            | ChangeReason.Delete ->
              let tbl' = SwitchTable.change tbl c_id (function
                  | None -> None
                  | Some eth_tbl ->
                    Some(EthTable.filter eth_tbl (fun ~key:_ ~data:v -> v = port))) in
              return tbl'
            | ChangeReason.Add
            | ChangeReason.Modify -> return tbl
          end
        | _ -> failwith "WHAT MESSAGE IS THIS???"
      end
    end

let main () =
  let open OF0x01Controller in
  create 6633
  >>= fun t ->
    Pipe.fold (listen t) ~init:SwitchTable.empty ~f:(switch t)

let _ = main ()
let _ = never_returns (Scheduler.go ())
