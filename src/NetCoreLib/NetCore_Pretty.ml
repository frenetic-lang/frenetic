open Format
open NetCore_Types.Internal
open NetCore_Pattern

let format_list fmt sep lst =
  let open Format in
  let rec loop fmt lst = match lst with
    | [x; y] -> fprintf fmt "@[%s%s@]@[%s@]" x sep y
    | [x] -> fprintf fmt "@[%s@]" x
    | [] ->  ()
    | x :: lst' -> fprintf fmt "@[%s%s@]@[%a@]" x sep loop lst' in
  fprintf fmt "@[%a@]" loop lst

let format_pattern fmt pat =
  let open Format in
  if is_all pat then
    fprintf fmt "all"
  else if is_empty pat then
    fprintf fmt "<none>"
  else 
    format_list fmt " && "
      [ DlAddrWildcard.to_string_exact "dlSrc = " pat.ptrnDlSrc;
        DlAddrWildcard.to_string_exact "dlDst = " pat.ptrnDlDst;
        DlTypWildcard.to_string_exact "dlTyp = " pat.ptrnDlType;
        DlVlanWildcard.to_string_exact "dlVlan = " pat.ptrnDlVlan;
        DlVlanPcpWildcard.to_string_exact "dlVlanPcp = " pat.ptrnDlVlanPcp;
        NwAddrWildcard.to_string_exact "nwSrc = " pat.ptrnNwSrc;
        NwAddrWildcard.to_string_exact "nwDst = " pat.ptrnNwDst;
        NwProtoWildcard.to_string_exact "nwProto = " pat.ptrnNwProto;
        NwTosWildcard.to_string_exact "nwTos = " pat.ptrnNwTos;
        TpPortWildcard.to_string_exact "tpSrc = " pat.ptrnTpSrc;
        TpPortWildcard.to_string_exact "tpDst = " pat.ptrnTpDst;
        PortWildcard.to_string_exact "inPort = " pat.ptrnInPort ]
        
let pattern_to_string x =
  let buf = Buffer.create 100 in
  let fmt = Format.formatter_of_buffer buf in
  Format.pp_set_margin fmt 80;
  format_pattern fmt x;
  Format.fprintf fmt "@?";
  Buffer.contents buf

let rec format_pred fmt pred = match pred with 
  | PrHdr pat -> fprintf fmt "HDR"
    (* fprintf fmt "@[PrHdr@;<1 2>@[%a@]@]" NetCore_Pattern.to_format pat *)
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

let rec format_pol fmt pol = match pol with
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

let port_to_string = function
  | Physical pid -> "Physical " ^ (string_of_int pid)
  | All -> "All"
  | Here -> "Here"

let value_to_string = function 
  | Pkt (sid, port, pkt, pay) ->
    Printf.sprintf "(%Ld, %s, %s, _)" 
      sid (port_to_string port) (Packet.packet_to_string pkt)
