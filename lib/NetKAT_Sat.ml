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
    | ZLessThan of zTerm * zTerm
    | ZGreaterThan of zTerm * zTerm
    | ZComment of string * zFormula

  type zDeclare = 
    | ZDeclareRule of zVar * (zVar list) * zFormula
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
      | SFunction _ -> 
        Printf.sprintf "_f%d" n 
      | SRelation _ -> 
	Printf.sprintf "_r%d" n
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
      "(_ BitVec 8)"
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

	 

  let rec serialize_term term : string = 
    match term with 
      | TUnit -> 
        "()"
      | TVar x -> 
	x
      | TInt n -> 
	Printf.sprintf "(_ bv%s 8)"
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
    | ZEquals (t1, t2) -> 
      Printf.sprintf "(equals %s %s)" (serialize_term t1) (serialize_term t2)
    | ZLessThan (t1, t2) -> 
      Printf.sprintf "(bvult %s %s)" (serialize_term t1) (serialize_term t2)
    | ZGreaterThan (t1, t2) -> 
      Printf.sprintf "(bvugt %s %s)" (serialize_term t1) (serialize_term t2)
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

  let define_z3_macro (name : string) (arglist : (zVar * zSort) list)  (rettype : zSort) (body : zFormula)  = 
    [ZDefineVar (name, SMacro (arglist, rettype), body)]

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
    let start = "starting_packet"
    let ending = "ending_packet"
    let inpkt = "inpkt"
    let midpkt = "midpkt"
    let outpkt = "outpkt"
    let q = "q"

  end
  open Z3macro
      
  let pervasives : string = 
    "
(declare-datatypes 
 () 
 ((Packet
   (packet
    (Switch "^serialize_sort SInt ^")
    (EthDst "^serialize_sort SInt ^")
    (EthType "^serialize_sort SInt ^")
    (Vlan "^serialize_sort SInt ^")
    (VlanPcp "^serialize_sort SInt ^")
    (IPProto "^serialize_sort SInt ^")
    (IP4Src "^serialize_sort SInt ^")
    (IP4Dst "^serialize_sort SInt ^")
    (TCPSrcPort "^serialize_sort SInt ^")
    (TCPDstPort "^serialize_sort SInt ^")
    (EthSrc "^serialize_sort SInt ^")
    (InPort "^serialize_sort SInt ^")))))
(declare-datatypes
 ()
 ((Hist 
    (hist-singleton (packet Packet))
    (hist (packet Packet) (rest-hist Hist))
    )))" ^ "\n" 
      

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

      
  let serialize_program p : string = 
    let ZProgram(ds) = p in 
    let ds' = List.flatten [[ZDeclareVar(Z3macro.start, SPacket);
			     ZDeclareVar(Z3macro.ending, SPacket);
			     ZDeclareVar(Z3macro.inpkt, SPacket);
			     ZDeclareVar(Z3macro.midpkt, SPacket);
			     ZDeclareVar(Z3macro.outpkt, SPacket);
			     ZDeclareVar(Z3macro.q, SRelation([SPacket; SPacket]))];
			    !fresh_cell; 
			    [ZToplevelComment("end initial declarations, commence dependent declarations\n")];
			    !decl_list;
			    [ZToplevelComment("End Definitions, Commence SAT expressions\n")]; 
			    ds] in 
    Printf.sprintf "%s%s\n%s\n"
      pervasives (intercalate serialize_declare "\n" ds') 
      (Printf.sprintf "(query (q %s %s) 
:default-relation smt_relation2
:engine PDR
:print-answer false)
" Z3macro.start Z3macro.ending)

  let solve prog : bool = 
    let s = (serialize_program prog) in
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
