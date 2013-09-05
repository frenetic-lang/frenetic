type header =
  | Header of SDN_Types.field
  | Switch

type header_val = VInt.t

type policy =
  | Drop
  | Id
  | Test of header * header_val
  | Mod of header * header_val
  | Neg of policy
  | Par of policy * policy
  | Seq of policy * policy

module HeaderMap = Map.Make (struct
    type t = header
    let compare = Pervasives.compare
  end)

type header_val_map = header_val HeaderMap.t

type packet = {
  headers : header_val_map;
  payload : SDN_Types.payload
}

module PacketSet = Set.Make (struct
    type t = packet

    (* First compare by headers, then payload. The payload comparison is a
       little questionable. However, this is safe to use in eval, since
       all output packets have the same payload as the input packet. *)
    let compare x y =
      let cmp = HeaderMap.compare Pervasives.compare x.headers y.headers in
      if cmp != 0 then
        cmp
      else
        Pervasives.compare x.payload y.payload
  end)

let rec eval (pkt:packet) (pol:policy) : PacketSet.t = match pol with
  | Drop -> PacketSet.empty
  | Id -> PacketSet.singleton pkt
  | Test (h, v) -> 
    if HeaderMap.find h pkt.headers = v then
      PacketSet.singleton pkt
    else
      PacketSet.empty
  | Mod (h, v) ->
    if HeaderMap.mem h pkt.headers then
      PacketSet.singleton { pkt with headers = HeaderMap.add h v pkt.headers }
    else
      raise Not_found (* for consistency with Test *)
  | Neg p ->
    PacketSet.diff (PacketSet.singleton pkt) (eval pkt p)
  | Par (pol1, pol2) ->
    PacketSet.union (eval pkt pol1) (eval pkt pol2)
  | Seq (pol1, pol2) ->
    let f pkt' set = PacketSet.union (eval pkt' pol2) set in
    PacketSet.fold f (eval pkt pol1) PacketSet.empty

module Formatting = struct

  open Format

  let header (fmt : formatter) (h : header) : unit = match h with
    | Header h' -> SDN_Types.format_field fmt h'
    | Switch -> pp_print_string fmt "switch"

  (* The type of the immediately surrounding context, which guides parenthesis-
     intersion. *)
  (* JNF: YES. This is the Right Way to pretty print. *)
  type context = SEQ | PAR | NEG | PAREN

  let rec pol (cxt : context) (fmt : formatter) (p : policy) : unit = match p with
    | Drop -> fprintf fmt "@[drop@]"
    | Id -> fprintf fmt "@[id@]"
    | Test (h, v) -> fprintf fmt "@[%a = %a@]" header h VInt.format v
    | Mod (h, v) -> fprintf fmt "@[%a <- %a@]" header h VInt.format v
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

let format_policy = Formatting.pol Formatting.PAREN

let string_of_policy = make_string_of format_policy
