open NetKAT_Types

module Formatting = struct
  open Format
    
  (* The type of the immediately surrounding policy_context, which
     guides parenthesis insertion. *)
    
  type predicate_context = OR_L | OR_R | AND_L | AND_R | NEG | PAREN_PR
      
  let format_header_val (fmt : formatter) (hv : header_val) (asgn : string) : unit = match hv with
    | Switch(n) -> fprintf fmt "@[switch %s %Lu@]" asgn n
    | Location(Physical n) -> fprintf fmt "@[port %s %lu@]" asgn n
    | Location(Pipe x) -> fprintf fmt "@[port %s pipe(%s)@]" asgn x
    | Location(Query x) -> fprintf fmt "@[port %s query(%s)@]" asgn x
    | EthSrc(n) -> fprintf fmt "@[ethSrc %s %s@]" asgn (Packet.string_of_mac n)
    | EthDst(n) -> fprintf fmt "@[ethDst %s %s@]" asgn (Packet.string_of_mac n)
    | Vlan(n) -> fprintf fmt "@[vlanId %s %d@]" asgn n
    | VlanPcp(n) -> fprintf fmt "@[vlanPcp %s %u@]" asgn n
    | EthType(n) -> fprintf fmt "@[ethTyp %s 0x%x@]" asgn n
    | IPProto(n) -> fprintf fmt "@[ipProto %s 0x%x@]" asgn n
    | IP4Src(n,32l) -> fprintf fmt "@[ipSrc %s %s@]" asgn (Packet.string_of_ip n)
    | IP4Dst(n,32l) -> fprintf fmt "@[ipDst %s %s@]" asgn (Packet.string_of_ip n)
    | IP4Src(n,m) -> fprintf fmt "@[ipSrc %s %s/%lu@]" asgn (Packet.string_of_ip n) m
    | IP4Dst(n,m) -> fprintf fmt "@[ipDst %s %s/%lu@]" asgn (Packet.string_of_ip n) m
    | TCPSrcPort(n) -> fprintf fmt "@[tcpSrcPort %s %u@]" asgn n
    | TCPDstPort(n) -> fprintf fmt "@[tcpDstPort %s %u@]" asgn n

  let rec pred (cxt : predicate_context) (fmt : formatter) (pr : pred) : unit = 
    match pr with
      | True -> 
        fprintf fmt "@[id@]"
      | False -> 
        fprintf fmt "@[drop@]"
      | (Test hv) -> 
        format_header_val fmt hv "="
      | Neg p' -> 
        begin match cxt with
          | PAREN_PR
          | NEG -> fprintf fmt "@[not %a@]" (pred NEG) p'
          | _ -> fprintf fmt "@[not@ @[(%a)@]@]" (pred PAREN_PR) p'
        end
      | And (p1, p2) ->
        begin match cxt with
          | PAREN_PR
          | OR_L
          | OR_R
          | AND_L -> 
	    fprintf fmt "@[%a and@ %a@]" (pred AND_L) p1 (pred AND_R) p2
          | _ -> 
	    fprintf fmt "@[(@[%a and@ %a@])@]" (pred AND_L) p1 (pred AND_R) p2
        end
      | Or (p1, p2) -> 
        begin match cxt with
          | PAREN_PR
          | OR_L -> fprintf fmt "@[%a or@ %a@]" (pred OR_L) p1 (pred OR_R) p2
          | _ -> fprintf fmt "@[(@[%a or@ %a@])@]" (pred OR_L) p1 (pred OR_R) p2
        end

  type policy_context = 
    | SEQ_L | SEQ_R 
    | PAR_L | PAR_R 
    | STAR 
    | PAREN

  let rec pol (cxt : policy_context) (fmt : formatter) (p : policy) : unit =
    match p with
      | Filter (True as pr) | Filter (False as pr) ->  
	pred PAREN_PR fmt pr
      | Filter pr -> 
	fprintf fmt "filter "; pred PAREN_PR fmt pr
      | Mod hv -> 
        format_header_val fmt hv ":="
      | Star p' -> 
        begin match cxt with
          | PAREN 
          | STAR -> fprintf fmt "@[%a*@]" (pol STAR) p' 
          | _ -> fprintf fmt "@[@[(%a)*@]@]" (pol PAREN) p'
        end
      | Union (p1, p2) -> 
        begin match cxt with
          | PAREN
          | PAR_L -> fprintf fmt "@[%a |@ %a@]" (pol PAR_L) p1 (pol PAR_R) p2
          | _ -> fprintf fmt "@[(@[%a |@ %a@])@]" (pol PAR_L) p1 (pol PAR_R) p2
        end
      | Seq (p1, p2) -> 
        begin match cxt with
          | PAREN
          | PAR_L
          | PAR_R
          | SEQ_L -> fprintf fmt "@[%a;@ %a@]" (pol SEQ_L) p1 (pol SEQ_R) p2
          | _ -> fprintf fmt "@[(@[%a;@ %a@])@]" (pol SEQ_L) p1 (pol SEQ_R) p2
        end
      | Link (sw,pt,sw',pt') ->
        fprintf fmt "@[%Lu@@%lu =>@ %Lu@@%lu@]"
          sw pt sw' pt'
end
  
let format_policy = Formatting.pol Formatting.PAREN

let format_pred = Formatting.pred Formatting.PAREN_PR
    
let string_of_policy = Util.make_string_of format_policy

let string_of_pred = Util.make_string_of format_pred

let rec pretty_assoc (p : policy) : policy = match p with
  | Filter _ -> p
  | Mod _ -> p
  | Link _ -> p
  | Union (p1, p2) -> pretty_assoc_par p
  | Seq (p1, p2) -> pretty_assoc_seq p
  | Star p' -> Star (pretty_assoc p')
and pretty_assoc_par (p : policy) : policy = match p with
  | Union (p1, Union (p2, p3)) ->
    Union (pretty_assoc_par (Union (p1, p2)), pretty_assoc_par p3)
  | Union (p1, p2) -> Union (pretty_assoc p1, pretty_assoc p2)
  | _ -> pretty_assoc p
and pretty_assoc_seq (p : policy) : policy = match p with
  | Seq (p1, Seq (p2, p3)) ->
    Seq (pretty_assoc_seq (Seq (p1, p2)), pretty_assoc_seq p3)
  | Seq (p1, p2) -> Seq (pretty_assoc p1, pretty_assoc p2)
  | _ -> pretty_assoc p    
