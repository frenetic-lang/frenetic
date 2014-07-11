open Async.Std
open Core.Std

open OpenFlow0x04
open OpenFlow0x04_Core
open OpenFlow0x04.Message

module OF0x04Controller = Async_OpenFlow0x04.Controller

module SwitchTable = Map.Make(OF0x04Controller.Client_id)

module EthTable = Map.Make(struct
  type t = Int64.t sexp_opaque with sexp
  let compare = Pervasives.compare
end)

type switchTable = portId EthTable.t SwitchTable.t

let switch
  (ctl : OF0x04Controller.t)
  (tbl : switchTable)
   evt =
  let ensure result response =
    begin match response with
      | `Sent _   -> return result
      | `Drop exn -> raise exn
    end in

  let learn c_id pi =
    let pkt = parse_payload pi.pi_payload in
    let rec findPort oxmMatch = 
      match oxmMatch with
        | [] -> failwith "no PhysicalPort"
        | (OxmInPhyPort t)::q -> t
        | t::q -> findPort q in
    let eth, port = pkt.Packet.dlSrc, (findPort pi.pi_ofp_match) in
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
      let fwd, buf = match pi.pi_payload with
        | Buffered (b_id, _) -> (false, Some b_id)
        | NotBuffered _ -> (true, None) in
      OF0x04Controller.send ctl c_id
        (1l, FlowModMsg (add_flow 1 ([OxmEthDst {m_value = eth; m_mask = None}]) ([ApplyActions [Output(PhysicalPort port)]]))
        )
        (* XXX(seliopu): can ensure asynchronously if not buffered, or if the
         * buffering is ignored completely.
         * *)
        >>= ensure (fwd, tbl') in

  let forward (tbl : switchTable) c_id t_id pi =
    let dst = (parse_payload pi.pi_payload).Packet.dlDst in
    let rec findPort oxmMatch = 
      match oxmMatch with
        | [] -> failwith "no PhysicalPort"
        | (OxmInPhyPort t)::q -> t
        | t::q -> findPort q in
    let port = findPort pi.pi_ofp_match in
    let out =
      match EthTable.find (SwitchTable.find_exn tbl c_id) dst with
        | None -> Flood
        | Some(p) -> PhysicalPort(p) in
    OF0x04Controller.send ctl c_id
      (t_id, PacketOutMsg {
          po_payload = pi.pi_payload;
          po_port_id = Some(port);
          po_actions = [ Output(out) ] }) in

  begin match evt with
    | `Connect c_id ->
      OF0x04Controller.send ctl c_id
        (0l, FlowModMsg (add_flow 1 
            ([])
            ([ApplyActions [Output(Controller 1024)]]))
        )
      >>= ensure (SwitchTable.add tbl c_id EthTable.empty)
    | `Disconnect (c_id, _) ->
      return (SwitchTable.remove tbl c_id)
    | `Message (c_id, msg) ->
      let t_id, msg = msg in
      begin match msg with
        | PacketInMsg pi ->
          learn c_id pi
            >>= (function
              | (true , tbl') -> forward tbl' c_id t_id pi >>= ensure tbl'
              | (false, tbl') -> return tbl')
        | Error err -> failwith (Error.to_string err)
        | PortStatusMsg ps ->
          let open PortStatus in
          let port = ps.desc.port_no in
          begin match ps.reason with
            | PortDelete ->
              let tbl' = SwitchTable.change tbl c_id (function
                  | None -> None
                  | Some eth_tbl ->
                    Some(EthTable.filter eth_tbl (fun ~key:_ ~data:v -> v = port))) in
              return tbl'
            | PortAdd
            | PortModify -> return tbl
          end
        | _ -> failwith "WHAT MESSAGE IS THIS???"
      end
    end

let main () =
  let open OF0x04Controller in
  create 6633 ()
  >>= fun t ->
    Pipe.fold (listen t) ~init:SwitchTable.empty ~f:(switch t)

let _ = main ()
let _ = never_returns (Scheduler.go ())
