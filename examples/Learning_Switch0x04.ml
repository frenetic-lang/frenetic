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

  let learn sw_id pi =
    let pkt = parse_payload pi.pi_payload in
    let rec findPort oxmMatch = 
      match oxmMatch with
        | [] -> failwith "no PhysicalPort"
        | (OxmInPort t)::q
        | (OxmInPhyPort t)::q -> t
        | t::q -> findPort q in
    let eth, port = pkt.Packet.dlSrc, (findPort pi.pi_ofp_match) in
    let tbl', present = match SwitchTable.find tbl sw_id with
      | None -> (SwitchTable.add tbl sw_id (EthTable.singleton eth port), false)
      | Some e_tbl ->
        match EthTable.find e_tbl eth with
          | None -> (SwitchTable.add tbl sw_id (EthTable.add e_tbl eth port), false)
          | Some port' when port = port' -> (tbl, true)
          | _ -> failwith "Inconsistent topology" in (* XXX(seliopou): exn *)
    if present then
      return (true, tbl')
    else
      let fwd, buf = match pi.pi_payload with
        | Buffered (b_id, _) -> (false, Some b_id)
        | NotBuffered _ -> (true, None) in
      OF0x04Controller.send ctl sw_id
        (1l, FlowModMsg (
        { mfCookie = val_to_mask 0L
        ; mfTable_id = 0
        ; mfCommand = AddFlow
        ; mfIdle_timeout = Permanent
        ; mfHard_timeout = Permanent
        ; mfPriority = 5
        ; mfBuffer_id = buf
        ; mfOut_port = None
        ; mfOut_group = None
        ; mfFlags = { fmf_send_flow_rem = false
                    ; fmf_check_overlap = false
                    ; fmf_reset_counts = false
                    ; fmf_no_pkt_counts = false
                    ; fmf_no_byt_counts = false }
        ; mfOfp_match = ([OxmEthDst {m_value = eth; m_mask = None}])
        ; mfInstructions = ([ApplyActions [Output(PhysicalPort port)]])
        }
        ))
        (* XXX(seliopu): can ensure asynchronously if not buffered, or if the
         * buffering is ignored completely.
         * *)
        >>= ensure (fwd, tbl') in

  let forward (tbl : switchTable) sw_id t_id pi =
    let dst = (parse_payload pi.pi_payload).Packet.dlDst in
    let rec findPort oxmMatch = 
      match oxmMatch with
        | [] -> failwith "no PhysicalPort"
        | (OxmInPort t)::q
        | (OxmInPhyPort t)::q -> t
        | t::q -> findPort q in
    let port = findPort pi.pi_ofp_match in
    let out =
      match EthTable.find (SwitchTable.find_exn tbl sw_id) dst with
        | None -> Flood
        | Some(p) -> PhysicalPort(p) in
    OF0x04Controller.send ctl sw_id
      (t_id, PacketOutMsg {
          po_payload = pi.pi_payload;
          po_port_id = Some(port);
          po_actions = [ Output(out) ] }) in

  begin match evt with
    | `Connect (sw_id, ()) ->
      OF0x04Controller.send ctl sw_id
        (0l, FlowModMsg (add_flow 1 
            ([])
            ([ApplyActions [Output(Controller 1024)]]))
        )
      >>= ensure (SwitchTable.add tbl sw_id EthTable.empty)
    | `Disconnect (sw_id, _) ->
      return (SwitchTable.remove tbl sw_id)
    | `Message (sw_id, msg) ->
      let t_id, msg = msg in
      begin match msg with
        | PacketInMsg pi ->
          learn sw_id pi
            >>= (function
              | (true , tbl') -> forward tbl' sw_id t_id pi >>= ensure tbl'
              | (false, tbl') -> return tbl')
        | Error err -> failwith (Error.to_string err)
        | PortStatusMsg ps ->
          let open PortStatus in
          let port = ps.desc.port_no in
          begin match ps.reason with
            | PortDelete ->
              let tbl' = SwitchTable.change tbl sw_id (function
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
