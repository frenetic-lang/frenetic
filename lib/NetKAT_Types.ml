type hdr =
  | DlSrc
  | DlDst
  | DlTyp
  | DlVlan
  | DlVlanPcp
  | NwSrc
  | NwDst
  | NwProto
  | NwTos
  | TpSrc
  | TpDst
  | Port
  | Switch

type hdrVal =
  | DlAddr of Packet.dlAddr
  | DlTypVal of Packet.dlTyp
  | DlVlanVal of Packet.dlVlan
  | DlVlanPcpVal of Packet.dlVlanPcp
  | NwAddr of Packet.nwAddr
  | NwTosVal of Packet.nwTos
  | TpPort of Packet.tpPort
  | PortVal of SDN_types.portId
  | SwitchVal of SDN_types.switchId

type pol =
  | Drop
  | Id
  | Test of hdr * hdrVal
  | Set of hdr * hdrVal 
  | Neg of pol
  | Par of pol * pol
  | Seq of pol * pol

module Formatting = struct

  type cxt = SEQ | PAR | NEG | PAREN

  open Format

  let hdr (fmt : formatter) (h : hdr) : unit = 
    pp_print_string fmt
      (match h with
        | DlSrc -> "dlSrc"
        | DlDst -> "dlDst"
        | DlTyp -> "dlTyp"
        | DlVlan -> "dlVlan"
        | DlVlanPcp -> "dlVlanPcp"
        | NwSrc -> "nwSrc"
        | NwDst -> "nwDst"
        | NwProto -> "nwProto"
        | NwTos -> "nwTos"
        | TpSrc -> "tpSrc"
        | TpDst -> "tpDst"
        | Port -> "port"
        | Switch -> "switch")

  let hdrVal (fmt : formatter) (v : hdrVal) : unit = match v with
    | DlAddr n -> fprintf fmt "%Ld" n
    | DlTypVal n -> fprintf fmt "%d" n
    | DlVlanVal None -> fprintf fmt "0xFFFF"     
    | DlVlanVal (Some n) -> fprintf fmt "%d" n
    | DlVlanPcpVal n -> fprintf fmt "%d" n
    | NwAddr n -> fprintf fmt "%ld" n
    | NwTosVal n -> fprintf fmt "%d" n
    | TpPort n -> fprintf fmt "%d" n
    | PortVal n -> SDN_types.format_portId fmt n
    | SwitchVal n -> SDN_types.format_switchId fmt n

  let rec pol (cxt : cxt) (fmt : formatter) (p : pol) : unit = match p with
    | Drop -> fprintf fmt "@[drop@]"
    | Id -> fprintf fmt "@[id@]"
    | Test (h, v) -> fprintf fmt "@[%a = %a@]" hdr h hdrVal v
    | Set (h, v) -> fprintf fmt "@[%a <- %a@]" hdr h hdrVal v
    | Neg p' -> begin match cxt with
      | PAREN -> fprintf fmt "@[!%a@]" (pol NEG) p'
      | _ -> fprintf fmt "@[!@[(%a)@]@]" (pol PAREN) p'
      end
    | Par (p1, p2) -> begin match cxt with
      | PAREN
      | PAR -> fprintf fmt "@[%a + %a@]" (pol PAR) p1 (pol PAR) p2
      | _ -> fprintf fmt "@[(@[%a + %a@])@]" (pol PAR) p1 (pol PAR) p2
      end
    | Seq (p1, p2) -> begin match cxt with
      | PAREN
      | SEQ
      | PAR -> fprintf fmt "@[%a ; %a@]" (pol SEQ) p1 (pol SEQ) p2
      | _ -> fprintf fmt "@[(@[%a ; %a@])@]" (pol SEQ) p1 (pol SEQ) p2
      end

end

let make_string_of formatter x =
  let open Format in
  let buf = Buffer.create 100 in
  let fmt = formatter_of_buffer buf in
  pp_set_margin fmt 80;
  formatter fmt x;
  fprintf fmt "@?";
  Buffer.contents buf

let format_pol = Formatting.pol Formatting.PAREN

let string_of_pol  = make_string_of format_pol
