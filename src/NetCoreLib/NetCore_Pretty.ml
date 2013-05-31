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

let match_modify_to_string
    (pr : 'a -> string) (lbl : string) (v : 'a match_modify) : string option =
  match v with
    | None -> None
    | Some (old, new_) -> 
      Some (Format.sprintf "%s:%s->%s" lbl (pr old) (pr new_))

let string_of_output (out : output) : string = 
    let mods =
      [ match_modify_to_string Packet.dlAddr_to_string "DlSrc" out.outDlSrc;
        match_modify_to_string Packet.dlAddr_to_string "DlDst" out.outDlDst;
        match_modify_to_string Packet.dlVlan_to_string "DlVlan" out.outDlVlan;
        match_modify_to_string Packet.dlVlanPcp_to_string "DlVlanPcp" out.outDlVlanPcp;
        match_modify_to_string Packet.string_of_ip "NwSrc" out.outNwSrc;
        match_modify_to_string Packet.string_of_ip "NwDst" out.outNwDst;
        match_modify_to_string Packet.nwTos_to_string "NwTos" out.outNwTos;
        match_modify_to_string string_of_int "TpSrc" out.outTpSrc;
        match_modify_to_string string_of_int "TpDst" out.outTpDst ] in
    let mods = String.concat ", " (List.fold_right (fun xo acc -> match xo with None -> acc | Some x -> x::acc) mods []) in
  (* "FWD" *) 
          if mods = "" then
          port_to_string out.outPort
          else
          Format.sprintf " %s<%s>"
          (port_to_string out.outPort)
          mods

let string_of_action_atom atom = match atom with
  | SwitchAction output -> (string_of_output output)
  | ControllerAction _ -> "controller"
  | ControllerQuery (time, f) -> 
    Printf.sprintf "ControllerQuery %f" time

let action_to_string output_list =
  match output_list with
    | [] -> "drop"
    | [a] -> string_of_action_atom a
    | _ ->
      Printf.sprintf "[%s]"
	(String.concat ", " (List.map string_of_action_atom output_list))

module Format = struct

  let rec pred fmt p = match p with
    | PrAnd (p1, p2) -> fprintf fmt "@[%a@ && %a@]" orpred p1 pred p2
    | _ -> orpred fmt p

  and orpred fmt p = match p with
    | PrAnd (p1, p2) -> fprintf fmt "@[%a@ || %a@]" apred p1 pred p2
    | _ -> apred fmt p

  and apred fmt p = match p with 
    | PrHdr ptrn -> format_pattern fmt ptrn
    | PrOnSwitch sw -> fprintf fmt "@[switch = %Lx@]" sw
    | PrNot p' -> fprintf fmt "@[!@ %a@]" apred p'
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
    | PoAction a -> fprintf fmt "@[%s@]" (action_to_string a)
    | PoFilter pr -> pred fmt pr
    | PoUnion _
    | PoSeq _
    | PoITE _ -> fprintf fmt "@[(%a)@]" pol p

end

let format_pol = Format.pol

let format_pred = Format.pred

let rec pred_to_string pred = 
  let buf = Buffer.create 100 in
  let fmt = formatter_of_buffer buf in
  pp_set_margin fmt 80;
  Format.pred fmt pred;
  fprintf fmt "@?";
  Buffer.contents buf

let rec pol_to_string pred = 
  let buf = Buffer.create 100 in
  let fmt = formatter_of_buffer buf in
  pp_set_margin fmt 80;
  format_pol fmt pred;
  fprintf fmt "@?";
  Buffer.contents buf

let value_to_string = function 
  | Pkt (sid, port, pkt, pay) ->
    Printf.sprintf "(%Ld, %s, %s, _)" 
      sid (port_to_string port) (Packet.packet_to_string pkt)
