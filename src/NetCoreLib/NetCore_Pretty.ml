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
  | Physical pid -> "Physical " ^ (string_of_int pid)
  | All -> "All"
  | Here -> "Here"

let format_pattern fmt pat =
  let open Format in
      if is_all pat then
        fprintf fmt "all"
      else if is_empty pat then
        fprintf fmt "<none>"
      else 
        format_list fmt " && "
          [ to_string_exact Packet.string_of_mac "dlSrc = " pat.ptrnDlSrc;
            to_string_exact Packet.string_of_mac "dlDst = " pat.ptrnDlDst;
            to_string_exact string_of_int "dlTyp = " pat.ptrnDlType;
            to_string_exact Packet.dlVlan_to_string "dlVlan = " pat.ptrnDlVlan;
            to_string_exact string_of_int "dlVlanPcp = " pat.ptrnDlVlanPcp;
            to_string_exact Packet.string_of_ip "nwSrc = " pat.ptrnNwSrc;
            to_string_exact Packet.string_of_ip "nwDst = " pat.ptrnNwDst;
            to_string_exact string_of_int "nwProto = " pat.ptrnNwProto;
            to_string_exact string_of_int "nwTos = " pat.ptrnNwTos;
            to_string_exact string_of_int "tpSrc = " pat.ptrnTpSrc;
            to_string_exact string_of_int "tpDst = " pat.ptrnTpDst;
            to_string_exact port_to_string "inPort = " pat.ptrnInPort ]
          
let pattern_to_string x =
  let buf = Buffer.create 100 in
  let fmt = Format.formatter_of_buffer buf in
  Format.pp_set_margin fmt 80;
  format_pattern fmt x;
  Format.fprintf fmt "@?";
  Buffer.contents buf

let rec format_pred fmt pred = match pred with 
  | PrHdr pat ->
    fprintf fmt "@[PrHdr@;<1 2>@[%a@]@]" format_pattern pat
  | PrOnSwitch sw ->
    fprintf fmt "@[PrOnSwitch %Lx@]" sw
  | PrOr (p1,p2) ->
    fprintf fmt "@[PrOr@;<1 2>@[(@[%a@],@ @[%a@])@]@]"
      format_pred p1 format_pred p2
  | PrAnd (p1,p2) -> 
    fprintf fmt "@[PrAnd@;<1 2>@[(@[%a@],@ @[%a@])@]@]" 
      format_pred p1 format_pred p2
  | PrNot p -> 
    fprintf fmt "@[PrNot@;<1 2>(@[%a@])@]" format_pred p
  | PrAll -> 
    pp_print_string fmt "PrAll"
  | PrNone -> 
    pp_print_string fmt "PrNone"

let rec pred_to_string pred = 
  let buf = Buffer.create 100 in
  let fmt = formatter_of_buffer buf in
  pp_set_margin fmt 80;
  format_pred fmt pred;
  fprintf fmt "@?";
  Buffer.contents buf


let match_modify_to_string
    (pr : 'a -> string) (lbl : string) (v : 'a match_modify) : string option =
  match v with
    | None -> None
    | Some (old, new_) -> 
      Some (Format.sprintf "%s:%s->%s" lbl (pr old) (pr new_))

let string_of_output (out : output) : string = 
    (* let mods = *)
    (*   [ match_modify_to_string dlAddr_to_string "DlSrc" out.outDlSrc; *)
    (*     match_modify_to_string dlAddr_to_string "DlDst" out.outDlDst; *)
    (*     match_modify_to_string dlVlan_to_string "DlVlan" out.outDlVlan; *)
    (*     match_modify_to_string dlVlanPcp_to_string "DlVlanPcp" out.outDlVlanPcp; *)
    (*     match_modify_to_string string_of_ip "NwSrc" out.outNwSrc; *)
    (*     match_modify_to_string string_of_ip "NwDst" out.outNwDst; *)
    (*     match_modify_to_string nwTos_to_string "NwTos" out.outNwTos; *)
    (*     match_modify_to_string string_of_int "TpSrc" out.outTpSrc; *)
    (*     match_modify_to_string string_of_int "TpDst" out.outTpDst ] in *)
    (* let mods = String.concat ", " (List.fold_right (fun xo acc -> match xo with None -> acc | Some x -> x::acc) mods []) in *)
  "FWD" (*
          if mods = "" then
          Format.sprintf "Fwd %s"
          (PortOrderedType.to_string out.outPort)
          else 
          Format.sprintf "Fwd %s<%s>"
          (PortOrderedType.to_string out.outPort)
          mods *)

let string_of_action_atom atom = match atom with
  | SwitchAction output -> "SwitchAction " ^ (string_of_output output)
  | ControllerAction _ -> "ControllerAction _"
  | ControllerQuery (time, f) -> 
    Printf.sprintf "ControllerQuery %f" time

let action_to_string output_list =
  Printf.sprintf "[%s]"
    (String.concat ", " (List.map string_of_action_atom output_list))



let rec format_pol fmt pol = match pol with
  | HandleSwitchEvent _ -> fprintf fmt "HandleSwitchEvent _"
  | PoAction a -> fprintf fmt "ACTION"
  (* fprintf fmt "@[PoAction@;<1 2>@[%s@]@]" (NetCore_Action.Output.to_string a) *)
  | PoFilter pr -> 
    fprintf fmt "@[PoFilter@;<1 2>(@[%a@])@]" format_pred pr
  | PoUnion (p1,p2) -> 
    fprintf fmt "@[PoUnion@;<1 2>@[(@[%a@],@ @[%a@])@]@]" format_pol p1
      format_pol p2
  | PoSeq (p1,p2) -> 
    fprintf fmt "@[PoSeq@;<1 2>@[(@[%a@],@ @[%a@])@]@]" format_pol p1
      format_pol p2
  | PoITE (pred, then_pol, else_pol) ->
    fprintf fmt "@[PoITE@;<1 2>@[(@[%a@],@ @[%a@],@ @[%a@])@]@]"
      format_pred pred format_pol then_pol format_pol else_pol

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
