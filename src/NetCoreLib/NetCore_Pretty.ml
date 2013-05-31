open Format
open NetCore_Types
open NetCore_Pattern

let to_string_exact = NetCore_Wildcard.to_string_exact

let format_list fmt sep lst =
  let open Format in
      let rec loop fmt lst = match lst with
        | [x; y] -> fprintf fmt "@[%s%s@]@[%s@]" x sep y
        | [x] -> fprintf fmt "@[%s@]" x
        | [] ->  ()
        | x :: lst' -> fprintf fmt "@[%s%s@]@[%a@]" x sep loop lst' in
      fprintf fmt "@[%a@]" loop lst

let port_to_string = function
  | Physical pid -> (string_of_int pid)
  | All -> "All"
  | Here -> "Here"

let format_pattern fmt pat =
  let open Format in
      if is_all pat then
        fprintf fmt "*"
      else if is_empty pat then
        fprintf fmt "none"
      else 
        format_list fmt " && "
          (List.filter (fun x -> not (x = "")) 
	     [ to_string_exact Packet.string_of_mac "srcmac" pat.ptrnDlSrc;
               to_string_exact Packet.string_of_mac "dstmac" pat.ptrnDlDst;
               to_string_exact string_of_int "frameType " pat.ptrnDlType;
               to_string_exact Packet.dlVlan_to_string "vlan" pat.ptrnDlVlan;
               to_string_exact string_of_int "dlVlanPcp" pat.ptrnDlVlanPcp;
               to_string_exact Packet.string_of_ip "srcip" pat.ptrnNwSrc;
               to_string_exact Packet.string_of_ip "dstip" pat.ptrnNwDst;
               to_string_exact string_of_int "nwProto" pat.ptrnNwProto;
               to_string_exact string_of_int "nwTos" pat.ptrnNwTos;
               to_string_exact string_of_int "tcpsrcport" pat.ptrnTpSrc;
               to_string_exact string_of_int "tcpdstport" pat.ptrnTpDst;
               to_string_exact port_to_string "inPort" pat.ptrnInPort ])
          
let pattern_to_string x =
  let buf = Buffer.create 100 in
  let fmt = Format.formatter_of_buffer buf in
  Format.pp_set_margin fmt 80;
  format_pattern fmt x;
  Format.fprintf fmt "@?";
  Buffer.contents buf




module Format = struct

  let match_modify pr lbl fmt mm = match mm with
    | None -> ()
    | Some (old, new_) -> fprintf fmt "@[@[%s@,->@,%s@];@ @]"
      (pr old) (pr new_)

  let output fmt (out : output) : unit =
    fprintf fmt "@[%a%a%a%a%a%a%a%a%a%s@]"
      (match_modify Packet.dlAddr_to_string "dlSrc") out.outDlSrc
      (match_modify Packet.dlAddr_to_string "dlDst") out.outDlDst
      (match_modify Packet.dlVlan_to_string "dlVlan") out.outDlVlan
      (match_modify Packet.dlVlanPcp_to_string "dlVlanPcp") out.outDlVlanPcp
      (match_modify Packet.string_of_ip "nwSrc") out.outNwSrc
      (match_modify Packet.string_of_ip "nwDst") out.outNwDst
      (match_modify Packet.nwTos_to_string "nwTos") out.outNwTos
      (match_modify string_of_int "tpSrc") out.outTpSrc
      (match_modify string_of_int "tpDst") out.outTpDst
      (port_to_string out.outPort)

  let action fmt action : unit = match action with
    | SwitchAction o -> output fmt o
    | ControllerAction _ -> fprintf fmt "controller"
    | ControllerQuery (time, f) -> fprintf fmt "ControllerQuery %f" time

  let rec action_list fmt lst = match lst with
    | [] -> fprintf fmt "drop"
    | [x] -> action fmt x
    | x :: lst' -> fprintf fmt "@[%a@ | %a@]" action x action_list lst'

  let rec pred fmt p = match p with
    | PrAnd (p1, p2) -> fprintf fmt "@[%a@ && %a@]" orpred p1 pred p2
    | _ -> orpred fmt p

  and orpred fmt p = match p with
    | PrAnd (p1, p2) -> fprintf fmt "@[%a@ || %a@]" apred p1 pred p2
    | _ -> apred fmt p

  and apred fmt p = match p with 
    | PrHdr ptrn -> format_pattern fmt ptrn
    | PrOnSwitch sw -> fprintf fmt "@[switch = %Lx@]" sw
    | PrNot p' -> fprintf fmt "@[!%a@]" apred p'
    | PrAll -> fprintf fmt "@[*@]"
    | PrNone -> fprintf fmt "@[none@]" 
    (* TODO(arjun): concrete syntax is "<none>", don't know how to escape *)
    | PrOr _
    | PrAnd _ -> fprintf fmt "@[(%a)@]" pred p

  let rec pol fmt p = match p with
    | PoSeq (p1, p2) -> fprintf fmt "@[@[%a;@ @]%a@]" cpol p1 seq_pol_list p2
    | PoUnion (p1, p2) -> fprintf fmt "@[%a@ |@ %a@]" cpol p1 par_pol_list p2
    | _ -> cpol fmt p

  and seq_pol_list fmt p = match p with
    | PoSeq (p1, p2) -> fprintf fmt "@[@[%a;@]%a@]" cpol p1 seq_pol_list p2
    | _ -> cpol fmt p

  and par_pol_list fmt p = match p with
    | PoUnion (p1, p2) -> fprintf fmt "@[@[%a;@]%a@]" cpol p1 par_pol_list p2
    | _ -> cpol fmt p

  and cpol fmt p = match p with
    | PoITE (pr, then_pol, else_pol) ->
	    fprintf fmt "@[if@ %a@;<1 2>@[then@;<1 2>%a@]@;<1 2>@[else@;<1 2>%a@]@]"
        pred pr cpol then_pol cpol else_pol
    | _ -> apol fmt p

  and apol fmt p = match p with
    | HandleSwitchEvent _ -> fprintf fmt "@[HandleSwitchEvent _@]"
    | PoAction a -> fprintf fmt "@[%a@]" action_list a
    | PoFilter pr -> pred fmt pr
    | PoUnion _
    | PoSeq _
    | PoITE _ -> fprintf fmt "@[(%a)@]" pol p

end

let format_pol = Format.pol

let format_pred = Format.pred

let mk_to_string formatter x =
  let buf = Buffer.create 100 in
  let fmt = formatter_of_buffer buf in
  pp_set_margin fmt 80;
  formatter fmt x;
  fprintf fmt "@?";
  Buffer.contents buf

let pred_to_string = mk_to_string Format.pred

let pol_to_string = mk_to_string Format.pol

let action_to_string = mk_to_string Format.action_list

let value_to_string = function 
  | Pkt (sid, port, pkt, pay) ->
    Printf.sprintf "(%Ld, %s, %s, _)" 
      sid (port_to_string port) (Packet.packet_to_string pkt)
