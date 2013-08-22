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
  | Int64 of Int64.t
  | Int48 of Int64.t
  | Int32 of Int32.t
  | Int16 of int
  | Int4 of int

type pol =
  | Drop
  | Id
  | Test of hdr * hdrVal
  | Set of hdr * hdrVal 
  | Neg of pol
  | Par of pol * pol
  | Seq of pol * pol

module HdrMap = Map.Make (struct
    type t = hdr
    let compare = Pervasives.compare
  end)

type hdrValMap = hdrVal HdrMap.t

type pkt = {
  headers : hdrValMap;
  payload : SDN_types.payload
}

module PktSet = Set.Make (struct
    type t = pkt

    (* First compare by headers, then payload. The payload comparison is a
       little questionable. However, this is safe to use in eval, since
       all output packets have the same payload as the input packet. *)
    let compare x y =
      let cmp = HdrMap.compare Pervasives.compare x.headers y.headers in
      if cmp != 0 then
        cmp
      else
        Pervasives.compare x.payload y.payload
  end)

let rec eval (pkt : pkt) (pol : pol) : PktSet.t = match pol with
  | Drop -> PktSet.empty
  | Id -> PktSet.singleton pkt
  | Test (h, v) -> 
    if HdrMap.find h pkt.headers = v then
      PktSet.singleton pkt
    else
      PktSet.empty
  | Set (h, v) ->
    if HdrMap.mem h pkt.headers then
      PktSet.singleton { pkt with headers = HdrMap.add h v pkt.headers }
    else
      raise Not_found (* for consistency with Test *)
  | Neg p ->
    PktSet.diff (PktSet.singleton pkt) (eval pkt p)
  | Par (pol1, pol2) ->
    PktSet.union (eval pkt pol1) (eval pkt pol2)
  | Seq (pol1, pol2) ->
    let f pkt' set = PktSet.union (eval pkt' pol2) set in
    PktSet.fold f (eval pkt pol1) PktSet.empty

module Formatting = struct

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
    | Int64 n -> fprintf fmt "%Ld" n
    | Int48 n -> fprintf fmt "%Ld" n
    | Int32 n -> fprintf fmt "%ld" n
    | Int16 n -> fprintf fmt "%d" n
    | Int4 n -> fprintf fmt "%d" n

  (* The type of the immediately surrounding context, which guides parenthesis-
     intersion. *)
  type cxt = SEQ | PAR | NEG | PAREN

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

let string_of_pol = make_string_of format_pol
