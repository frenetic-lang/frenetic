open Types

module Formatting = struct
  open Format


  let parens (on : bool) (fmt : formatter) (thunk : unit -> unit) : unit =
  (match on with
     | false -> thunk ()
     | true  -> 
         pp_open_box fmt 0;
         pp_print_string fmt "(";
         thunk ();
         pp_print_string fmt ")";
         pp_close_box fmt ())

  let format_value (fmt : formatter) (v : VInt.t) : unit =
    match v with
      | VInt.Int64 n -> 
	if (Int64.of_int (-1) = n) then 
	  fprintf fmt "<none>"
        else 
	  VInt.format fmt v
      | _ -> VInt.format fmt v

  let format_header (fmt : formatter) (h : header) : unit = match h with
    | Header h' -> SDN_Types.format_field fmt h'
    | Switch -> fprintf fmt "switch"


  (* The type of the immediately surrounding policy_context, which guides parenthesis-
     insertion. *)
  type predicate_context =
    | PAREN_PR 
    | OR_L  | OR_R 
    | AND_L | AND_R
    | NEG

  let print_pred_paren (cxt : predicate_context) (t : pred) : bool =
    match t with 
      | And _ -> cxt > AND_L
      | Or _  -> cxt > OR_L
      | _ -> false


  let rec pred (cxt : predicate_context) (fmt : formatter) (pr : pred) : unit = 
    parens (print_pred_paren cxt pr) fmt (fun () ->
    match pr with
      | True         -> fprintf fmt "@[id@]"
      | False        -> fprintf fmt "@[drop@]"
      | Test (h, v)  -> fprintf fmt "@[%a = %a@]" format_header h format_value v
      | Neg p'       -> fprintf fmt "@[not %a@]" (pred NEG) p'
      | And (p1, p2) -> fprintf fmt "@[%a and@ %a@]" (pred AND_L) p1 (pred AND_R) p2
      | Or (p1, p2)  -> fprintf fmt "@[%a or@ %a@]"  (pred OR_L)  p1 (pred OR_R)  p2)


  type policy_context = 
    | PAREN
    | PAR_L    | PAR_R 
    | CHOICE_L | CHOICE_R 
    | SEQ_L    | SEQ_R 
    | STAR 


  let print_pol_paren (cxt : policy_context) (t : policy) : bool =
    match t with 
      | Seq _ -> cxt > SEQ_L
      | Par _  -> cxt > PAR_L
      | Choice _ -> cxt > CHOICE_L
      | _ -> false


  let rec pol (cxt : policy_context) (fmt : formatter) (p : policy) : unit =
  parens (print_pol_paren cxt p) fmt (fun () ->
    match p with
      | Filter (True as pr) | Filter (False as pr) ->  
	pred PAREN_PR fmt pr
      | Filter pr -> fprintf fmt "filter "; pred PAREN_PR fmt pr
      | Mod (h, v) -> fprintf fmt "@[%a := %a@]" format_header h format_value v
      | Star p' -> fprintf fmt "@[%a*@]" (pol STAR) p' 
      | Par (p1, p2) -> fprintf fmt "@[%a |@ %a@]" (pol PAR_L) p1 (pol PAR_R) p2
      | Seq (p1, p2) -> fprintf fmt "@[%a;@ %a@]" (pol SEQ_L) p1 (pol SEQ_R) p2
      | Choice (p1, p2) -> fprintf fmt "@[%a +@ %a@]" (pol CHOICE_L) p1 (pol CHOICE_R) p2
      | Link (sw,pt,sw',pt') -> 
        fprintf fmt "@[%a@@%a =>@ %a@@%a@]"
          format_value sw format_value pt
          format_value sw' format_value pt')
end
  
let format_policy = Formatting.pol Formatting.PAREN

let format_pred = Formatting.pred Formatting.PAREN_PR
  
let format_header = Formatting.format_header
  
 let header_to_string = Util.make_string_of Formatting.format_header
  
let value_to_string = Util.make_string_of Formatting.format_value
  
let string_of_policy = Util.make_string_of format_policy

let string_of_pred = Util.make_string_of format_pred

let rec pretty_assoc (p : policy) : policy = match p with
  | Filter _ -> p
  | Mod _ -> p
  | Link _ -> p
  | Par (p1, p2) -> pretty_assoc_par p
  | Seq (p1, p2) -> pretty_assoc_seq p
  | Choice (p1, p2) -> pretty_assoc_choice p
  | Star p' -> Star (pretty_assoc p')
and pretty_assoc_par (p : policy) : policy = match p with
  | Par (p1, Par (p2, p3)) ->
    Par (pretty_assoc_par (Par (p1, p2)), pretty_assoc_par p3)
  | Par (p1, p2) -> Par (pretty_assoc p1, pretty_assoc p2)
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
    
