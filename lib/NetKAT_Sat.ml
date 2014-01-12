open Packet
open Types
open Util
open Unix

module type ParameterizedOnInts = 
sig 
  val ints : VInt.t list
end

(* The [Sat] module provides a representation of formulas in
   first-order logic, a representation of packets, and a function for
   testing their satisfiability. *)

module Sat_Syntax = struct 

    type zVar = string
	
    type zSort = 
      | SPacket
      | SInt
      | SBool
      | SRelation of (zSort list)
      | SFunction of (zSort list) * zSort
      | SMacro of ((zVar * zSort) list) * zSort
	  
    type zTerm = 
      | TUnit 
      | TVar of zVar
      | TInt of Int64.t
      | TApp of zTerm * (zTerm list)
	  
    type zFormula =
      | ZNoop
      | ZTrue
      | ZFalse 
      | ZAnd of zFormula list
      | ZOr of zFormula list
      | ZEquals of zTerm * zTerm
      | ZNotEquals of zTerm * zTerm
      | ZComment of string * zFormula
	  
    type zDeclare = 
      | ZDeclareRule of zVar * (zVar list) * zFormula
      | ZDeclareVar of zVar * zSort
      | ZDefineVar of zVar * zSort * zFormula
      | ZDeclareAssert of zFormula
      | ZToplevelComment of string
	  
    type zProgram = 
      | ZProgram of zDeclare list

end

module Sat_Utils = struct
  open Sat_Syntax
    (*number of bits required to represent number *)
  let number_of_bits n = 
    let rec number_of_bits n acc = 
      if (n == 0)
      then acc
      else number_of_bits (n asr 1) (acc + 1) in
    number_of_bits n 0

let collect_constants pol : (VInt.t list) = 
  let module VInt_set = Set.Make(struct 
    let compare = Pervasives.compare
    type t = VInt.t
  end) in
  
  let combine a b = VInt_set.union a b in
  let empty = VInt_set.empty in
  let single = VInt_set.singleton in
  let elems l = List.fold_left (fun a x -> VInt_set.add x a) empty l in
  let rec collect_constants pol = 
    let rec collect_pred_constants pred = 
      match pred with
	| True -> empty
	| False -> empty
	| Test (_, v) -> single v
	| And (a,b) -> combine (collect_pred_constants a) (collect_pred_constants b)
	| Or (a,b) -> combine (collect_pred_constants a) (collect_pred_constants b)
	| Neg p -> collect_pred_constants p
    in
    match pol with
      | Link (s1, p1, s2, p2) -> elems [s1;p1;s2;p2]
      | Filter pred -> collect_pred_constants pred
      | Mod (_, v) -> single v
      | Par (l, r) -> combine (collect_constants l) (collect_constants r)
      | Seq (f, s) -> combine (collect_constants f) (collect_constants s)
      | Star p -> collect_constants p
      | Choice (a, b) -> combine (collect_constants a) (collect_constants b) in
  VInt_set.elements (collect_constants pol)

let rec remove_links (pol : 'a) : 'a = 
  let make_transition (switch1, port1) (switch2, port2) : policy =     
    Seq (Filter (And (Test (Switch, switch1), Test (Header SDN_Types.InPort, port1))), 
	 Seq (Mod (Switch , switch2) , Mod (Header SDN_Types.InPort, port2))) in
  match pol with 
    | Link (s1,p1,s2,p2) -> make_transition (s1, p1) (s2, p2)
    | Filter _ -> pol
    | Mod _ -> pol
    | Par (l, r) -> Par (remove_links l, remove_links r)
    | Seq (f, s) -> Seq (remove_links f, remove_links s)
    | Star p -> Star (remove_links p)
    | Choice _ -> failwith "choice not supported"

    open SDN_Types
    let serialize_header (header: header) : string = 
      match header with
	| Header InPort -> 
	  "InPort"
	| Header EthSrc -> 
	  "EthSrc"
	| Header EthDst -> 
	  "EthDst"
	| Header EthType ->  
	  "EthType"
	| Header Vlan ->  
	  "Vlan"
	| Header VlanPcp ->
	  "VlanPcp"
	| Header IPProto ->  
	  "IPProto"
	| Header IP4Src ->  
	  "IP4Src"
	| Header IP4Dst ->  
	  "IP4Dst"
	| Header TCPSrcPort ->  
	  "TCPSrcPort"
	| Header TCPDstPort ->  
	  "TCPDstPort"
	| Switch -> 
	  "Switch"
	    
    let serialize_comment c = 
      Printf.sprintf "%s" c
	
	    
    let all_fields =
      [ Header InPort 
      ; Header EthSrc
      ; Header EthDst
      ; Header EthType
      ; Header Vlan
      ; Header VlanPcp
      ; Header IPProto
      ; Header IP4Src
      ; Header IP4Dst
      ; Header TCPSrcPort
      ; Header TCPDstPort
      ; Switch 
      ]
	
    let encode_header (header: header) (pkt:zVar) : zTerm =
      match header with
	| Header InPort -> 
          TApp (TVar (serialize_header header), [TVar pkt])
	| Header EthSrc -> 
          TApp (TVar (serialize_header header), [TVar pkt])
	| Header EthDst -> 
          TApp (TVar (serialize_header header), [TVar pkt])
	| Header EthType ->  
          TApp (TVar (serialize_header header), [TVar pkt])
	| Header Vlan ->  
          TApp (TVar (serialize_header header), [TVar pkt])
	| Header VlanPcp ->
          TApp (TVar (serialize_header header), [TVar pkt])
	| Header IPProto ->  
          TApp (TVar (serialize_header header), [TVar pkt])
	| Header IP4Src ->  
          TApp (TVar (serialize_header header), [TVar pkt])
	| Header IP4Dst ->  
          TApp (TVar (serialize_header header), [TVar pkt])
	| Header TCPSrcPort ->  
          TApp (TVar (serialize_header header), [TVar pkt])
	| Header TCPDstPort ->  
          TApp (TVar (serialize_header header), [TVar pkt])
	| Switch -> 
          TApp (TVar (serialize_header header), [TVar pkt])

    let encode_vint (v: VInt.t): zTerm = 
      TInt (VInt.get_int64 v)
	    
end

module Sat = 
  functor (Int_List : ParameterizedOnInts) -> struct
      
    open Sat_Utils
    open Sat_Syntax

    let bitvec_size = Sat_Utils.number_of_bits ((List.length Int_List.ints) + 1)
	  
    (* fresh variables *)
    let fresh_cell = ref []
    let macro_list_top = ref []
    let macro_list_bottom = ref []
    
      
    let fresh s = 
      let l = !fresh_cell in  
      let n = List.length l in 
      let x = match s with
	| SPacket -> 
          Printf.sprintf "_pkt%d" n
	| SInt -> 
          Printf.sprintf "_n%d" n
	| SFunction _ -> 
          Printf.sprintf "_f%d" n 
	| SRelation _ -> 
	  Printf.sprintf "_r%d" n
	| _ -> failwith "not implemented in fresh" in 
      fresh_cell := ZDeclareVar(x,s)::l;
      x
	
    let rec serialize_sort = function
      | SInt -> 
	Printf.sprintf "(_ BitVec %d)" bitvec_size
      | SPacket -> 
	"Packet"
      | SBool ->
	"Bool"
      | SFunction(sortlist,sort2) -> 
	Printf.sprintf "(%s) %s" 
          (intercalate serialize_sort " " sortlist)
          (serialize_sort sort2)
      | SMacro(args,ret) -> 
	let serialize_arglist args = 
	  (intercalate (fun (a, t) -> Printf.sprintf "(%s %s)" a (serialize_sort t)) " " args) in
	Printf.sprintf "(%s) %s"
	  (serialize_arglist args)
	  (serialize_sort ret)
      | SRelation (sortlist) ->
	Printf.sprintf "(%s)"
	  (intercalate serialize_sort " " sortlist)
	  
    let serialize_arglist args = 
      (intercalate (fun (a, t) -> Printf.sprintf "(%s %s)" a (serialize_sort t)) " " args)
	
    let tInt_to_string = 
      let numbers_map = Hashtbl.create 0 in
      List.iteri (fun i x -> Hashtbl.add numbers_map (VInt.get_int64 x) i) Int_List.ints;
      let tInt_to_string = function
	| TInt n -> (Printf.sprintf "(_ bv%d %d)" (Hashtbl.find numbers_map n) bitvec_size)
	| _ -> failwith "wasn't a tint" in
      tInt_to_string
	
    let rec serialize_term term : string = 
      match term with 
	| TUnit -> 
          "()"
	| TVar x -> 
	  x
	| TInt n -> 
	  Printf.sprintf "%s"
            (tInt_to_string term)
	| TApp (term1, terms) -> 
	  Printf.sprintf "(%s %s)" (serialize_term term1) (intercalate serialize_term " " terms)	    

    let rec serialize_formula = function
      | ZNoop -> ""
      | ZTrue -> 
	Printf.sprintf "true"
      | ZFalse -> 
	Printf.sprintf "false"
      | ZEquals (t1, t2) -> 
      (*readability hack: remove "= true" case *)
	(match t1, t2 with
	  | TVar "true", t -> serialize_term t
	  | t, TVar "true" -> serialize_term t
	  | t1, t2 -> Printf.sprintf "(equals %s %s)" (serialize_term t1) (serialize_term t2))
      | ZNotEquals (t1, t2) -> 
      (*readability hack: remove "= true" case *)
	(match t1, t2 with
	  | TVar "true", t -> serialize_term t
	  | t, TVar "true" -> serialize_term t
	  | t1, t2 -> Printf.sprintf "(not (equals %s %s))" (serialize_term t1) (serialize_term t2))
      | ZAnd([]) -> 
	Printf.sprintf "true"
      | ZAnd([f]) -> 
	Printf.sprintf "%s" (serialize_formula f)
      | ZAnd(f::fs) -> 
	Printf.sprintf "(and %s %s)" (serialize_formula f) (serialize_formula (ZAnd(fs)))
      | ZOr([]) -> 
	Printf.sprintf "false"
      | ZOr([f]) -> 
	Printf.sprintf "%s" (serialize_formula f)
      | ZOr(f::fs) -> 
	Printf.sprintf "(or %s %s)" (serialize_formula f) (serialize_formula (ZOr(fs)))
      | ZComment(c, f) -> 
	Printf.sprintf "\n;%s\n%s\n; END %s\n" c (serialize_formula f) c

    let serialize_declare d = 
      match d with 
	| ZToplevelComment(c) -> 
	  Printf.sprintf "\n;%s" (Str.global_replace (Str.regexp_string "\n") "\n;" c)
	| ZDefineVar (x, s, b) -> 
	  Printf.sprintf "(define-fun %s %s %s)" x (serialize_sort s) (serialize_formula b)
	| ZDeclareVar (x, s) ->
          (match s with 
            | SFunction _ -> Printf.sprintf "(declare-fun %s %s)" x (serialize_sort s)
	    | SRelation _ -> Printf.sprintf "(declare-rel %s %s)" x (serialize_sort s)
	    | SMacro _ -> failwith "macros should be in ZDefineVar"
	    | SPacket -> 
	      "(declare-var "^x^" "^(serialize_sort s)^")" 
            | _ -> Printf.sprintf "(declare-var %s %s)" x (serialize_sort s)
	  )
	| ZDeclareAssert(f) -> 
          Printf.sprintf "(assert %s)" (serialize_formula f)
	| ZDeclareRule(sym, vars, body) ->
	  Printf.sprintf "(rule (=> %s (%s %s)))"
	    (serialize_formula body)
	    sym
	    (intercalate (fun x -> x) " " vars)


    let serialize_program pervasives p query: string = 
      let ZProgram(ds) = p in 
      let ds' = List.flatten [!fresh_cell;
			      !macro_list_top;
			      [ZToplevelComment("end initial declarations, commence dependent declarations\n")];
			      !macro_list_bottom;
			      [ZToplevelComment("End Definitions, Commence SAT expressions\n")]; 
			      ds] in 
      Printf.sprintf "%s\n%s\n%s\n"
	pervasives
	(intercalate serialize_declare "\n" ds') 
	query


    let define_z3_macro (name : string) (arglist : (zVar * zSort) list)  (rettype : zSort) (body : zFormula)  = 
      [ZDefineVar (name, SMacro (arglist, rettype), body)]
	
    let z3_macro, z3_macro_top = 
      let z3_macro_picklocation put_at_top (name : string) (arglist : (zVar * zSort) list) (rettype : zSort)(body : zFormula) : zTerm = 
	let name = name in
	let new_macro = (define_z3_macro name arglist rettype body) in
	(if put_at_top then
	    macro_list_top := new_macro @ (!macro_list_top)
	 else
	    macro_list_bottom := new_macro @ (!macro_list_bottom));
	TVar name in      
      let z3_macro = z3_macro_picklocation false in
      let z3_macro_top = z3_macro_picklocation true in
      z3_macro, z3_macro_top


    let solve pervasives prog query: bool = 
      let s = (serialize_program pervasives prog query) in
      let z3_out,z3_in = open_process "z3 -in -smt2 -nw" in 
      let _ = output_string z3_in s in
      let _ = flush z3_in in 
      let _ = close_out z3_in in 
      let b = Buffer.create 17 in 
      (try
	 while true do
           Buffer.add_string b (input_line z3_out);
           Buffer.add_char b '\n';
	 done
       with End_of_file -> ());
      Buffer.contents b = "sat\n"
  end
