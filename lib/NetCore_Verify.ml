open Packet
open Frenetic_List
open NetCore_Types
open Unix

(* JNF: This function belongs somewhere else. *)
let map_option f = function
  | None -> None
  | Some x -> Some (f x)

(* The [Sat] module provides a representation of formulas in
   first-order logic, a representation of packets, and a function for
   testing their satisfiability. *)
module Sat = struct

  type zVar = string

  type zSort = 
    | SPacket
    | SInt
    | SFunction of zSort * zSort
    | SRelation of zSort list

  type zTerm = 
    | TVar of zVar
    | TInt of Int64.t
    | TLocatedPacket of switchId * portId * packet
    | TFunction of zVar * zTerm list

  type zFormula =
    | ZTrue
    | ZFalse 
    | ZNot of zFormula
    | ZAnd of zFormula list
    | ZOr of zFormula list
    | ZEquals of zTerm * zTerm
	
  type zDeclaration = 
    | ZVarDeclare of zVar * zSort
    | ZSortDeclare of zVar * (zVar * (zVar * zSort) list) list 
    | ZAssertDeclare of zFormula

  type zProgram = 
    | ZProgram of zDeclaration list
	
  let fresh_cell = ref []

  let fresh sort = 
    let l = !fresh_cell in  
    let n = List.length l in 
    let x = match sort with
      | SPacket -> Printf.sprintf "_pkt%d" n 
      | SInt -> Printf.sprintf "_n%d" n
      | SFunction _ -> Printf.sprintf "_f%d" n
      | SRelation _ -> Printf.sprintf "_R%d" n in 
    fresh_cell := ZVarDeclare(x,sort)::l;
    x

  let serialize_located_packet (sw,pt,pkt) = 
    Printf.sprintf "(Packet %s %s %s %s)" 
      (Int64.to_string sw) 
      (Int32.to_string pt)
      (Int64.to_string pkt.dlSrc)
      (Int64.to_string pkt.dlDst)

  let rec serialize_sort = function
    | SPacket -> 
      "Packet"
    | SInt -> 
      "Int"
    | SFunction(sort1,sort2) -> 
      Printf.sprintf "(%s) %s" 
	(serialize_sort sort1) 
	(serialize_sort sort2)
    | SRelation(sorts) -> 
      Printf.sprintf "(%s)"
	(intercalate serialize_sort " " sorts)
	
  let rec serialize_term = function 
    | TVar x -> 
      x
    | TLocatedPacket (sw,pt,pkt) -> 
      serialize_located_packet (sw,pt,pkt)
    | TInt n -> 
      Printf.sprintf "%s" 
	(Int64.to_string n)
    | TFunction (f, terms) -> 
      Printf.sprintf "(%s %s)" f 
	(intercalate serialize_term " " terms)

  let rec serialize_formula = function
    | ZTrue -> 
      Printf.sprintf "true"
    | ZFalse -> 
      Printf.sprintf "false"
    | ZNot f1 -> 
      Printf.sprintf "(not %s)" (serialize_formula f1)
    | ZEquals (t1, t2) -> 
      Printf.sprintf "(equals %s %s)" (serialize_term t1) (serialize_term t2)
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

  let serialize_declaration = function
    | ZSortDeclare (name, constructorList) ->
      let serialize_field (field,sort) = 
	Printf.sprintf "(%s %s)" field (serialize_sort sort) in
      let serialize_constructor (name, fields) = 
	Printf.sprintf "(%s %s)" name (intercalate serialize_field " " fields) in 
      Printf.sprintf "(declare-datatypes () ((%s %s)))" 
	name (intercalate serialize_constructor " " constructorList)
    | ZVarDeclare (x, sort) ->
      let decl = match sort with 
	| SFunction _ -> "fun"
	| SRelation _ -> "rel"
	| _ -> "var" in 
      Printf.sprintf "(declare-%s %s %s)" decl x (serialize_sort sort)
    | ZAssertDeclare(f) -> 
      Printf.sprintf "(assert %s)" (serialize_formula f)

  let init_decls : zDeclaration list = 
    [ ZSortDeclare
	("Packet", [("packet", [ ("PSwitch", SInt)
                               ; ("PInPort", SInt)
                               ; ("PDlSrc", SInt)
                               ; ("PDlDst", SInt) ])])
    ; ZVarDeclare
      ("Switch", SFunction(SPacket, SInt))
    ; ZVarDeclare
      ("InPort", SFunction(SPacket, SInt))
    ; ZVarDeclare
      ("DlSrc", SFunction(SPacket, SInt))
    ; ZVarDeclare
      ("DlDst", SFunction(SPacket, SInt))
    ]

  let serialize_program (ZProgram (decls)) = 
    Printf.sprintf 
      "%s\n%s\n%s\n(check-sat)"
      (intercalate serialize_declaration "\n" init_decls)
      (intercalate serialize_declaration "\n" (!fresh_cell))
      (intercalate serialize_declaration "\n" decls) 

  let solve prog = 
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

module Topology = struct

  type link = Link of OpenFlow0x01.switchId * OpenFlow0x01.portId
      
  type topology = Topology of (link * link) list
      
  let reverse_edge (sp1, sp2) = (sp2, sp1)
    
  let bidirectionalize (Topology topo) = 
    Topology 
      (List.fold_left 
	 (fun acc edge -> edge::(reverse_edge edge)::acc) 
	 [] topo)

  (* JNF: placeholder. Need to find all-pairs max shortest path. *)
  let diameter _ = 3

end

module Verify = struct
  open Sat
  open Topology
    
  let packet_field (field:string) (pkt:zVar) (num:zTerm) : zFormula = 
    ZEquals(TFunction(field, [TVar pkt]), num)
      
  let encode_pattern (pat:ptrn) (pkt:zVar) : zFormula = 
    let map_wildcard f = function
      | WildcardExact x -> Some (f x)
      | WildcardAll -> None
      | WildcardNone -> Some (ZFalse) in 
    ZAnd 
      (filter_map
	 (fun x -> x)
	 [ map_wildcard (fun mac -> packet_field "DlSrc" pkt (TInt mac)) pat.ptrnDlSrc
	 ; map_wildcard (fun mac -> packet_field "DlDst" pkt (TInt mac)) pat.ptrnDlDst
	 ; map_wildcard 
           (function 
             (* TODO: PseudoPorts *)
             | All -> ZTrue
             | Here -> ZTrue
             | Physical pt -> packet_field "InPort" pkt (TInt (Int64.of_int32 pt))
	     | Queue _ -> ZTrue)
           pat.ptrnInPort ])

  let rec encode_predicate (pr:pred) (pkt:zVar) : zFormula = match pr with 
    | Everything -> 
      ZTrue
    | Nothing ->
      ZFalse
    | OnSwitch sw -> 
      packet_field "Switch" pkt (TInt sw)
    | Not pr1 ->
      ZNot(encode_predicate pr1 pkt)
    | And (pr1, pr2) ->
      ZAnd([encode_predicate pr1 pkt;
            encode_predicate pr2 pkt])
    | Or(pr1, pr2) -> 
      ZOr([encode_predicate pr1 pkt;
           encode_predicate pr2 pkt])
    | Hdr ptrn -> 
      encode_pattern ptrn pkt
          
  let equal_field (field:string) (pkt1:zVar) (pkt2:zVar) : zFormula list = 
    let num = TVar (fresh SInt) in 
    [ packet_field field pkt1 num
    ; packet_field field pkt2 num ]
      
  let equals (fields:string list) (pkt1:zVar) (pkt2:zVar) : zFormula list = 
    List.fold_left (fun acc field -> equal_field field pkt1 pkt2 @ acc) [] fields
      
  let topology_forwards (Topology topo:topology) (pkt1:zVar) (pkt2:zVar) : zFormula = 
    let eq = equals ["DlSrc"; "DlDst"] pkt1 pkt2 in 
    ZAnd (List.fold_left 
            (fun acc (Link(s1, p1), Link(s2, p2)) ->   
              eq 
              @ [ packet_field "Switch" pkt1 (TInt s1)
		; packet_field "InPort" pkt1 (TInt (Int64.of_int p1))
		; packet_field "Switch" pkt2 (TInt s2)
		; packet_field "InPort" pkt2 (TInt (Int64.of_int p2))]
              @ acc)
            [] topo)

  let output_forwards (out:output) (pkt1:zVar) (pkt2:zVar) : zFormula = 
    ZOr (filter_map
           (fun x -> x)
           [ map_option (fun (_,mac) -> packet_field "DlSrc" pkt2 (TInt mac)) out.outDlSrc
           ; map_option (fun (_,mac) -> packet_field "DlDst" pkt2 (TInt mac)) out.outDlDst
           ; Some (match out.outPort with 
           (* TODO: PseudoPorts *)
             | All -> ZNot(ZAnd(equal_field "InPort" pkt1 pkt2))
             | Here -> ZAnd(equal_field "InPort" pkt1 pkt2)
             | Physical pt -> packet_field "InPort" pkt2 (TInt (Int64.of_int32 pt))
	     | Queue _ -> ZFalse)])
      
  let action_atom_forwards (act:action_atom) (pkt1:zVar) (pkt2:zVar) : zFormula = match act with 
    | SwitchAction out -> 
      output_forwards out pkt1 pkt2
    | _ -> 
      ZFalse 
	
  let rec policy_forwards (pol:pol) (pkt1:zVar) (pkt2:zVar) : zFormula = 
    match pol with
      | Action (acts) -> 
	ZOr(List.map (fun act -> action_atom_forwards act pkt1 pkt2) acts)
      | Filter pr ->
	let eq = equals ["Switch"; "InPort"; "DlSrc"; "DlDst"] pkt1 pkt2 in 
	ZAnd(encode_predicate pr pkt1::eq)
      | Union (pol1, pol2) ->
	ZOr([ policy_forwards pol1 pkt1 pkt2
            ; policy_forwards pol2 pkt1 pkt2 ])
      | Seq (pol1, pol2) ->
	let pkt' = fresh SPacket in 
	ZAnd([ policy_forwards pol1 pkt1 pkt'
             ; policy_forwards pol2 pkt' pkt2])
      | ITE(pr,pol1,pol2) -> 
	let pol' = Union (Seq(Filter pr, pol1), Seq(Filter (Not pr), pol2)) in 
	policy_forwards pol' pkt1 pkt2
      | _ -> 
	failwith "policy_forwards: not yet implemented"

  let rec forwards (k:int) (topo:topology) (pol:pol) (pkt1:zVar) (pkt2:zVar) : zFormula = 
    if k = 0 then 
      ZAnd(equals ["Switch"; "InPort"; "DlSrc"; "DlDst"] pkt1 pkt2)
    else
      let pkt' = fresh SPacket in 
      let pkt'' = fresh SPacket in 
      ZOr [ ZAnd [ policy_forwards pol pkt1 pkt'
		 ; topology_forwards topo pkt' pkt''
		 ; forwards (k-1) topo pol pkt'' pkt2 ]
          ; forwards (k-1) topo pol pkt1 pkt2 ]
end

let topo = ref (Topology.Topology [])

let check str inp pol outp oko = 
  let x = Sat.fresh Sat.SPacket in 
  let y = Sat.fresh Sat.SPacket in 
  let prog = 
    Sat.ZProgram [ Sat.ZAssertDeclare (Verify.encode_predicate inp x)
		 ; Sat.ZAssertDeclare (Verify.forwards (Topology.diameter !topo) !topo pol x y)
		 ; Sat.ZAssertDeclare (Verify.encode_predicate outp y) ] in 
  match oko, Sat.solve prog with 
    | Some ok, sat -> 
      if ok = sat then 
	()
      else
	Printf.printf "[Verify.check %s: expected %b got %b]\n%!" str ok sat
    | None, sat -> 
      Printf.printf "[Verify.check %s: %b]\n%!" str sat
      
