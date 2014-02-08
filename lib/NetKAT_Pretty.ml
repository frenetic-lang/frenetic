open NetKAT_Types

module Formatting = struct
  open Format
    
  (* The type of the immediately surrounding policy_context, which
     guides parenthesis insertion. *)
    
  type predicate_context = OR_L | OR_R | AND_L | AND_R | NEG | PAREN_PR
      
  (* TODO(nimish) : Can be changed in SDN_Types.format_feild, will save
     duplication of effort and can be carried into NetCore pretty parser *)
  let format_field (fmt : formatter) (f : SDN_Types.field) : unit =
    Format.fprintf fmt
      (match f with
        | SDN_Types.InPort ->     "port"
        | SDN_Types.EthSrc ->     "ethSrc"
        | SDN_Types.EthDst ->     "ethDst"
        | SDN_Types.EthType ->    "ethTyp"
        | SDN_Types.Vlan ->       "vlanId"
        | SDN_Types.VlanPcp ->    "vlanPcp"
        | SDN_Types.IP4Src ->     "ipSrc"
        | SDN_Types.IP4Dst ->     "ipDst"
        | SDN_Types.IPProto ->    "ipProto"
        | SDN_Types.TCPSrcPort -> "tcpSrcPort"
        | SDN_Types.TCPDstPort -> "tcpDstPort")
      
  let format_value (fmt : formatter) (v : VInt.t) : unit =
    match v with
      | VInt.Int64 n -> 
	if (Int64.of_int (-1) = n) then 
	  fprintf fmt "<none>"
        else 
	  VInt.format fmt v
      | _ -> VInt.format fmt v

  let format_header (fmt : formatter) (h : header) : unit = match h with
    | Header h' -> format_field fmt h'
    | Switch -> fprintf fmt "switch"

  let rec pred (cxt : predicate_context) (fmt : formatter) (pr : pred) : unit = 
    match pr with
      | True -> 
        fprintf fmt "@[id@]"
      | False -> 
        fprintf fmt "@[drop@]"
      | (Test (h, v)) -> 
        fprintf fmt "@[%a = %a@]" 
	  format_header h 
	  format_value v
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
    | CHOICE_L | CHOICE_R 
    | STAR 
    | PAREN

  let rec pol (cxt : policy_context) (fmt : formatter) (p : policy) : unit =
    match p with
      | Filter (True as pr) | Filter (False as pr) ->  
	pred PAREN_PR fmt pr
      | Filter pr -> 
	fprintf fmt "filter "; pred PAREN_PR fmt pr
      | Mod (h, v) -> 
        fprintf fmt "@[%a := %a@]" format_header h format_value v
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
	  | CHOICE_L
	  | CHOICE_R 
          | SEQ_L -> fprintf fmt "@[%a;@ %a@]" (pol SEQ_L) p1 (pol SEQ_R) p2
          | _ -> fprintf fmt "@[(@[%a;@ %a@])@]" (pol SEQ_L) p1 (pol SEQ_R) p2
        end
      | Choice (p1, p2) -> 
        begin match cxt with
          | PAREN
          | CHOICE_L ->  
	    fprintf fmt "@[%a +@ %a@]" (pol CHOICE_L) p1 (pol CHOICE_R) p2
          | _ -> 
	    fprintf fmt "@[(@[%a +@ %a@])@]" (pol CHOICE_L) p1 (pol CHOICE_R) p2
        end
      | Link (sw,pt,sw',pt') ->
        fprintf fmt "@[%Lu@@%a =>@ %Lu@@%a@]"
          sw format_value pt
          sw' format_value pt'
end
  
let format_policy = Formatting.pol Formatting.PAREN

let format_pred = Formatting.pred Formatting.PAREN_PR
  
let format_header = Formatting.format_header
  
 let header_to_string = NetKAT_Util.make_string_of Formatting.format_header
  
let value_to_string = NetKAT_Util.make_string_of Formatting.format_value
  
let string_of_policy = NetKAT_Util.make_string_of format_policy

let string_of_pred = NetKAT_Util.make_string_of format_pred

let rec pretty_assoc (p : policy) : policy = match p with
  | Filter _ -> p
  | Mod _ -> p
  | Link _ -> p
  | Union (p1, p2) -> pretty_assoc_par p
  | Seq (p1, p2) -> pretty_assoc_seq p
  | Choice (p1, p2) -> pretty_assoc_choice p
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
and pretty_assoc_choice (p : policy) : policy = match p with
  | Choice (p1, Choice (p2, p3)) ->
    Choice (pretty_assoc_choice (Choice (p1, p2)), pretty_assoc_choice p3)
  | Choice (p1, p2) -> Choice (pretty_assoc p1, pretty_assoc p2)
  | _ -> pretty_assoc p
    
