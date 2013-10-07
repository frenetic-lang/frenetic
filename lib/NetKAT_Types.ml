type header =
  | Header of SDN_Types.field
  | Switch

type header_val = VInt.t

type pred = 
  | True
  | False
  | Test of header*header_val
  | And of pred*pred
  | Or of pred*pred
  | Neg of pred

type policy =
  | Filter of pred
  | Mod of header*header_val
  | Par of policy*policy
  | Seq of policy*policy
  | Star of policy

let id = Filter True
let drop = Filter False

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

let rec eval_pred (pkt:packet) (pr:pred) : bool = match pr with
  | True -> true
  | False -> false
  | Test (h, v) -> 
    if HeaderMap.find h pkt.headers = v then
      true
    else
      false
  | And (pr1, pr2) -> eval_pred pkt pr1 && eval_pred pkt pr2
  | Or (pr1, pr2) -> eval_pred pkt pr1 || eval_pred pkt pr2
  | Neg pr1 -> not (eval_pred pkt pr1)

let rec eval (pkt:packet) (pol:policy) : PacketSet.t = match pol with
  | Filter pr -> 
    if eval_pred pkt pr then 
      PacketSet.singleton pkt 
    else 
      PacketSet.empty
  | Mod (h, v) ->
    if HeaderMap.mem h pkt.headers then
      PacketSet.singleton { pkt with headers = HeaderMap.add h v pkt.headers }
    else
      raise Not_found (* for consistency with Test *)
  | Par (pol1, pol2) ->
    PacketSet.union (eval pkt pol1) (eval pkt pol2)
  | Seq (pol1, pol2) ->
    let f pkt' set = PacketSet.union (eval pkt' pol2) set in
    PacketSet.fold f (eval pkt pol1) PacketSet.empty
  | Star pol -> 
    let rec loop acc = 
      let f pkt' set = PacketSet.union (eval pkt' pol) set in 
      let acc' = PacketSet.fold f acc PacketSet.empty in 
      if PacketSet.equal acc acc' then acc else loop acc' in 
    loop (PacketSet.singleton pkt)

module Formatting = struct

  open Format

  (* 
    TODO : Can be changed in SDN_Types.format_feild, will save duplication of 
           effort and can be carried into NetCore pretty parser *)
  let format_field (fmt : formatter) (f : SDN_Types.field) : unit =
      pp_print_string fmt
      (match f with
        | SDN_Types.InPort ->     "inPort"
        | SDN_Types.EthType ->    "dlTyp"
        | SDN_Types.EthSrc ->     "dlSrc"
        | SDN_Types.EthDst ->     "dlDst"
        | SDN_Types.Vlan ->       "vlan"
        | SDN_Types.VlanPcp ->    "vlanPcp"
        | SDN_Types.IPProto ->    "nwProto"
        | SDN_Types.IP4Src ->     "srcIP"
        | SDN_Types.IP4Dst ->     "dstIP"
        | SDN_Types.TCPSrcPort -> "tcpSrcPort"
        | SDN_Types.TCPDstPort -> "tcpDstPort")

  let format_field_value (fmt : formatter) (v : VInt.t) : unit =
    (*if (Int64.of_int (NetKAT_Parser.vlan_none) = VInt.get_int64 v) then pp_print_string fmt "<none>" *)
    if (Int64.of_int (-1) = VInt.get_int64 v) then pp_print_string fmt "<none>"
    else VInt.format fmt v


  let header (fmt : formatter) (h : header) : unit = match h with
    (* | Header h' -> SDN_Types.format_field fmt h' *)
    | Header h' -> format_field fmt h'
    | Switch -> pp_print_string fmt "switch"


  type predicate_context = OR_L | OR_R | AND_L | AND_R | NEG | PAREN_PR

  let rec pred (cxt : predicate_context) (fmt : formatter) (pr : pred) : unit = 
    match pr with
    | True -> 
      fprintf fmt "@[pass@]"
    | False -> 
      fprintf fmt "@[drop@]"
    | (Test (h, v)) -> 
      fprintf fmt "@[%a = %a@]" header h format_field_value v

    | Neg p' -> 
      begin match cxt with
        | PAREN_PR
        | NEG -> fprintf fmt "@[!%a@]" (pred NEG) p'
        | _ -> fprintf fmt "@[!@[(%a)@]@]" (pred PAREN_PR) p'
      end

    | And (p1, p2) -> 
      begin match cxt with
        | PAREN_PR
        | OR_L
        | OR_R
        | AND_L -> fprintf fmt "@[%a && %a@]" (pred AND_L) p1 (pred AND_R) p2
        | _ -> fprintf fmt "@[(@[%a && %a@])@]" (pred AND_L) p1 (pred AND_R) p2
      end

    | Or (p1, p2) -> 
      begin match cxt with
        | PAREN_PR
        | OR_L -> fprintf fmt "@[%a || %a@]" (pred OR_L) p1 (pred OR_R) p2
        | _ -> fprintf fmt "@[(@[%a || %a@])@]" (pred OR_L) p1 (pred OR_R) p2
      end


  (* The type of the immediately surrounding policy_context, which guides parenthesis-
     intersion. *)
  (* JNF: YES. This is the Right Way to pretty print. *)
  type policy_context = SEQ_L | SEQ_R | PAR_L | PAR_R | STAR | PAREN


  let rec pol (cxt : policy_context) (fmt : formatter) (p : policy) : unit =
    match p with
    | Filter pr -> 
      (match pr with
         | True 
         | False -> pred PAREN_PR fmt pr
         | _ -> pp_print_string fmt "filter "; pred PAREN_PR fmt pr)

    | Mod (h, v) -> 
      fprintf fmt "@[%a -> %a@]" header h format_field_value v

    | Star p' -> 
      begin match cxt with
        | PAREN 
	| STAR ->  fprintf fmt "@[%a*@]" (pol STAR) p' 
        | _ -> fprintf fmt "@[@[(%a)*@]@]" (pol PAREN) p'
      end

    | Par (p1, p2) -> 
      begin match cxt with
        | PAREN
        | SEQ_L
        | SEQ_R
        | PAR_L -> fprintf fmt "@[%a + %a@]" (pol PAR_L) p1 (pol PAR_R) p2
        | _ -> fprintf fmt "@[(@[%a + %a@])@]" (pol PAR_L) p1 (pol PAR_R) p2
      end

    | Seq (p1, p2) -> 
      begin match cxt with
        | PAREN
        | SEQ_L -> fprintf fmt "@[%a ; %a@]" (pol SEQ_L) p1 (pol SEQ_R) p2
        | _ -> fprintf fmt "@[(@[%a ; %a@])@]" (pol SEQ_L) p1 (pol SEQ_R) p2
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

let string_of_vint = make_string_of VInt.format 

let string_of_header = make_string_of Formatting.header

let format_policy = Formatting.pol Formatting.PAREN

let string_of_policy = make_string_of format_policy
