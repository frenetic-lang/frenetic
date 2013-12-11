open Packet
open Types
open Util
open Unix


(* The [Sat] module provides a representation of formulas in
   first-order logic, a representation of packets, and a function for
   testing their satisfiability. *)
module Sat = struct

  type zVar = string

  type zSort = 
    | SPacket
    | SInt
    | SSet 
    | SBool
    | SFunction of (zSort list) * zSort
    | SMacro of ((zVar * zSort) list) * zSort

  type zTerm = 
    | TUnit 
    | TVar of zVar
    | TInt of Int64.t
    | TPkt of Topology.switchId * Topology.portId * Packet.packet
    | TApp of zTerm * (zTerm list)

  type zFormula =
    | ZNoop
    | ZTerm of zTerm
    | ZTrue
    | ZFalse 
    | ZNot of zFormula
    | ZAnd of zFormula list
    | ZOr of zFormula list
    | ZEquals of zFormula * zFormula
    | ZComment of string * zFormula
    | ZForall of ((zVar * zSort) list) * zFormula
(*    | ZExists of ((zVar * zSort) list) * zFormula *)
    | ZIf of zFormula * zFormula * zFormula
    | ZApp of zFormula * (zFormula list)

  type zDeclare = 
    | ZDeclareVar of zVar * zSort
    | ZDefineVar of zVar * zSort * zFormula
    | ZDeclareAssert of zFormula
    | ZToplevelComment of string

  type zProgram = 
    | ZProgram of zDeclare list

  (* fresh variables *)
  let fresh_cell = ref []
  let decl_list = ref []

  let fresh_named s str = 
    let l = !fresh_cell in  
    let x = match s with
      | SPacket -> 
        Printf.sprintf "_pkt%s" str
      | SInt -> 
        Printf.sprintf "_n%s" str
      | SSet -> 
        Printf.sprintf "_s%s" str
      | SFunction _ -> 
        Printf.sprintf "_f%s" str 
      | _ -> failwith "not implemented in fresh" in 
    fresh_cell := ZDeclareVar(x,s)::l;
    x


  let fresh s = 
    let l = !fresh_cell in  
    let n = List.length l in 
    let x = match s with
      | SPacket -> 
        Printf.sprintf "_pkt%d" n
      | SInt -> 
        Printf.sprintf "_n%d" n
      | SSet -> 
        Printf.sprintf "_s%d" n 
      | SFunction _ -> 
        Printf.sprintf "_f%d" n 
      | _ -> failwith "not implemented in fresh" in 
    fresh_cell := ZDeclareVar(x,s)::l;
    x




  (* serialization *)
  let serialize_located_packet (sw,pt,pkt) = 
    Printf.sprintf "(Packet %s %s %s %s)" 
      (Int64.to_string (VInt.get_int64 sw)) 
      (Int32.to_string (VInt.get_int32 pt))
      (Int64.to_string pkt.dlSrc)
      (Int64.to_string pkt.dlDst)


  let rec serialize_sort = function
    | SInt -> 
      "Int"
    | SPacket -> 
      "Packet"
    | SBool ->
      "Bool"
    | SSet -> 
      Printf.sprintf "Set"
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

  let serialize_arglist args = 
    (intercalate (fun (a, t) -> Printf.sprintf "(%s %s)" a (serialize_sort t)) " " args)

	 

  let rec serialize_term term : string = 
    match term with 
      | TUnit -> 
        "()"
      | TVar x -> 
	x
      | TPkt (sw,pt,pkt) -> 
	serialize_located_packet (sw,pt,pkt)
      | TInt n -> 
	Printf.sprintf "%s" 
          (Int64.to_string n)
      | TApp (term1, terms) -> 
	Printf.sprintf "(%s %s)" (serialize_term term1) (intercalate serialize_term " " terms)

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

  let rec serialize_formula = function
    | ZNoop -> ""
    | ZTrue -> 
      Printf.sprintf "true"
    | ZFalse -> 
      Printf.sprintf "false"
    | ZNot f1 -> 
      Printf.sprintf "(not %s)" (serialize_formula f1)
    | ZEquals (t1, t2) -> 
      Printf.sprintf "(equals %s %s)" (serialize_formula t1) (serialize_formula t2)
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
    | ZForall (args, form) ->
      Printf.sprintf "(forall (%s) %s)" (serialize_arglist args) (serialize_formula form)
(*    | ZExists (args, form) ->
      Printf.sprintf "(exists (%s) %s)" (serialize_arglist args) (serialize_formula form) *)
    | ZTerm t -> serialize_term t
    | ZIf (i, t, e) -> Printf.sprintf "(ite %s %s %s)" 
      (serialize_formula i) (serialize_formula t) (serialize_formula e)
    | ZApp (term1, terms) -> 
      Printf.sprintf "(%s %s)" (serialize_formula term1) (intercalate serialize_formula " " terms)

  let serialize_declare d = 
    match d with 
      | ZToplevelComment(c) -> 
	Printf.sprintf "\n;%s" c 
      | ZDefineVar (x, s, b) -> 
	Printf.sprintf "(define-fun %s %s %s)" x (serialize_sort s) (serialize_formula b)
      | ZDeclareVar (x, s) ->
        (match s with 
          | SFunction _ -> Printf.sprintf "(declare-fun %s %s)" x (serialize_sort s)
	  | SMacro _ -> failwith "macros should be in ZDefineVar"
	  | SPacket -> 
	    "(declare-var "^x^" "^(serialize_sort s)^")" 
          | _ -> Printf.sprintf "(declare-var %s %s)" x (serialize_sort s)
	)
      | ZDeclareAssert(f) -> 
        Printf.sprintf "(assert %s)" (serialize_formula f)

  let define_z3_macro (name : string) (arglist : (zVar * zSort) list)  (rettype : zSort) (body : zFormula)  = 
    [ZDefineVar (name, SMacro (arglist, rettype), body)]


  let zApp x = (fun l -> ZTerm (TApp (x, l)))

  let z3_macro, z3_macro_top = 
    let z3_macro_picklocation put_at_top (name : string) (arglist : (zVar * zSort) list) (rettype : zSort)(body : zFormula) : zTerm = 
      let l = !fresh_cell in
      let name = name (* ^ "_" ^ to_string (gensym ()) *) in
      let new_macro = (define_z3_macro name arglist rettype body) in
      (if put_at_top then
	fresh_cell := new_macro @ l
       else
	  decl_list := new_macro @ (!decl_list));
      TVar name in
      
    let z3_macro = z3_macro_picklocation false in
    let z3_macro_top = z3_macro_picklocation true in
    z3_macro, z3_macro_top


    
  module Z3macro = struct
    let nopacket_s = "nopacket"
    let nopacket = (ZTerm (TVar nopacket_s)) 

  end
  open Z3macro
      
  let pervasives : string = 
    "
(declare-datatypes 
 () 
 ((Packet
   (nopacket)
   (packet 
    (PreviousPacket Packet)
    (Switch Int) 
    (EthDst Int) 
    (EthType Int) 
    (Vlan Int) 
    (VlanPcp Int) 
    (IPProto Int) 
    (IP4Src Int) 
    (IP4Dst Int) 
    (TCPSrcPort Int) 
    (TCPDstPort Int) 
    (EthSrc Int) 
    (InPort Int)))))" ^ "\n" 
      
      
      
  let serialize_program p : string = 
    let ZProgram(ds) = p in 
    let ds' = List.flatten [!fresh_cell; 
			    [ZToplevelComment("end initial declarations, commence dependent declarations\n")];
			    !decl_list;
			    [ZToplevelComment("End Definitions, Commence SAT expressions\n")]; 
			    ds] in 
    Printf.sprintf "%s%s\n(check-sat)\n"
      pervasives (intercalate serialize_declare "\n" ds') 

  let solve prog : bool = 
    let s = serialize_program prog in 
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
