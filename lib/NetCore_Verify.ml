open Packet
open NetCore_Types
open NetCore_Util
open Unix

module W = NetCore_Wildcard
module P = NetCore_Pattern

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
    | TPkt of switchId * portId * packet
    | TApp of zVar * zTerm

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
    | TPkt (sw,pt,pkt) -> 
      serialize_located_packet (sw,pt,pkt)
    | TInt n -> 
      Printf.sprintf "%s" 
        (Int64.to_string n)
    | TApp (f, term) -> 
      Printf.sprintf "(%s %s)" f (serialize_term term)

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
        ("EthSrc", SFunction(SPacket, SInt))
    ; ZVarDeclare
        ("EthDst", SFunction(SPacket, SInt))
    ; ZVarDeclare
        ("EthType", SFunction(SPacket, SInt))
    ; ZVarDeclare
        ("Vlan", SFunction(SPacket, SInt))
    ; ZVarDeclare
        ("VlanPcp", SFunction(SPacket, SInt))
    ; ZVarDeclare
        ("IPProto", SFunction(SPacket, SInt))
    ; ZVarDeclare
        ("IP4Src", SFunction(SPacket, SInt))
    ; ZVarDeclare
        ("IP4Dst", SFunction(SPacket, SInt))
    ; ZVarDeclare
        ("TCPSrcPort", SFunction(SPacket, SInt))
    ; ZVarDeclare
        ("TCPDstPort", SFunction(SPacket, SInt))
    ]

  let serialize_program (ZProgram (decls)) = 
    Printf.sprintf 
      "%s\n%s\n%s\n(check-sat)"
      (intercalate serialize_declaration "\n" init_decls)
      (intercalate serialize_declaration "\n" (!fresh_cell))
      (intercalate serialize_declaration "\n" decls) 

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
	(*Printf.eprintf "%s" s;*)
    Buffer.contents b = "sat\n"
end

module NetKAT_Graph = struct
  open NetCore_Digraph
  open Sat
  open SDN_Types
  open NetKAT_Types

module Node =
struct
  type t = VInt.t * VInt.t
  let compare = Pervasives.compare
  let to_dot n = Printf.sprintf " we've not yet defined this completely. " 
  let to_string = to_dot
end

module Link =
struct
  open VInt

  module V = Node
  type v = Node.t
  type l = {
    switchA : VInt.t;
    portA : VInt.t;
    switchB : VInt.t;
    portB : VInt.t;
  }
  type t = v * v * l

  let compare = Pervasives.compare
  let blank = {
    switchA = Int64 Int64.zero;
    portA = Int64 Int64.zero;
    switchB = Int64 Int64.zero;
    portB = Int64 Int64.zero
  }

  (* Constructors and mutators *)
  let mk_edge s d l = (s,d,l)

  (* Accessors *)
  let src (s,d,l) = s
  let dst (s,d,l) = d
  let label (s,d,l) = l


  let name (s,d,_) =
    Printf.sprintf "%s_%s" (Node.to_string s) (Node.to_string d)
  let string_of_label (s,d, l) =
    Printf.sprintf "{srcswtch = %Ld; srcprt = %Ld; dstswtch = %Ld; dstprt = %Ld;}"
      (get_int64 l.switchA) (get_int64 l.portA) (get_int64 l.switchB) (get_int64 l.portB)
  let to_dot (s, d,l) =
    let s = Node.to_dot s in
    let d = Node.to_dot d in
    Printf.sprintf "%s -> %s [label=\"%s\"]" s d (string_of_label (s,d,l))
  let to_string = to_dot
end

module EdgeOrd = struct
  type t = Link.t
  let compare = Pervasives.compare
end

module type Topology_S = sig
  type t
  val create : unit -> t
  val get_vertices : t -> Node.t list
  val get_edges : t -> Link.t list
  val add_edge : t -> Link.t -> t
  val add_edges : t -> Link.t list -> t
  val add_vertex : t -> Node.t -> t
  val del_vertex : t -> Node.t -> t
  val merge : t -> t -> t
  val incoming : t -> Node.t -> Link.t list
  val outgoing : t -> Node.t -> Link.t list
  val to_dotty : string -> t -> string
end

module Topology : Topology_S = struct
  include Digraph.Make(Link)

  let to_dotty s g =
    let es = get_edges g in
    let strs = NetCore_Util.list_intercalate (fun e ->
      Link.to_dot e
    ) "\n" es
    in
    Printf.sprintf "digraph %s {\n%s\n}" s strs
end


module EdgeSet = NetCore_Util.Setplus.Make(EdgeOrd)

module EdgeMap = NetCore_Util.Mapplus.Make(EdgeOrd)

    (* note: this is brittle *)
    (* assumes input is of the form (p;t)* *)
    (* assumes t is of the form:
       switch = n, port = m, switch=n', port=m' + t *)

  (* dummy, waiting for BASU.  BASU!!! *)
  let build_graph a b = 4

  let assert_vint (vint : VInt.t) : VInt.t = vint

  let parse_graph ptstar = 
	let graph = Topology.create() in
    let rec parse_links (pol: policy): Link.t list = 
	  let assemble switch1 port1 switch2 port2 : Link.t= 
		let (node1: Node.t) = (assert_vint switch1, assert_vint port1) in
			 let (node2: Node.t) = (assert_vint switch2, assert_vint port2) in
			 let (label: Link.l) = {Link.switchA=switch1;
									Link.portA=port1;
									Link.switchB=switch2;
									Link.portB=port2 } in
			 (node1, node2, label)
	  in
      match pol with
		| Seq (Seq (Test (Switch, switch1), Test (Header InPort, port1)), 
			   (Seq (Mod (Switch ,switch2) , Mod (Header InPort, port2))))
		  -> (assemble switch1 port1 switch2 port2) :: []
		  
		| Par
			(Seq (Seq (Test (Switch, switch1), Test (Header InPort, port1)), 
				  (Seq (Mod (Switch ,switch2) , Mod (Header InPort, port2)))), t)
		  -> (assemble switch1 port1 switch2 port2):: (parse_links t)
		| _ -> failwith "unimplemented"

(*END INNER FUNCTION*)

	in
    match ptstar with
      | Star (Seq (p, t)) -> Topology.add_edges graph (parse_links t)
      | _ -> failwith "graph parsing assumes input is of the form (p;t)*"

end


module Verify = struct
  open Sat
  open SDN_Types
  open NetKAT_Types

  let all_fields =
      [ Header InPort 
      ; Header EthType
      ; Header EthSrc
      ; Header EthDst
      ; Header Vlan
      ; Header VlanPcp
      ; Header IPProto
      ; Header IP4Src
      ; Header IP4Dst
      ; Header TCPSrcPort
      ; Header TCPDstPort
      ; Switch 
]

  (* Bring header field names inline with SDN types*)
  let encode_header (header: header) (pkt: zVar): zTerm =
    match header with
      | Header InPort -> TApp ("InPort", TVar pkt)
      | Header EthType ->  TApp ("EthType", TVar pkt)
      | Header EthSrc -> TApp ("EthSrc", TVar pkt)
      | Header EthDst -> TApp ("EthDst", TVar pkt)
      | Header Vlan ->  TApp ("Vlan", TVar pkt)
      | Header VlanPcp ->  TApp ("VlanPcp", TVar pkt)
      | Header IPProto ->  TApp ("IPProto", TVar pkt)
      | Header IP4Src ->  TApp ("IP4Src", TVar pkt)
      | Header IP4Dst ->  TApp ("IP4Dst", TVar pkt)
      | Header TCPSrcPort ->  TApp ("TCPSrcPort", TVar pkt)
      | Header TCPDstPort ->  TApp ("TCPDstPort", TVar pkt)
      | Switch -> TApp ("Switch", TVar pkt)

  let equal_field (pkt1: zVar) (pkt2: zVar) (except_fields:header list): zFormula =
    ZAnd (List.fold_left 
	    (fun acc hd -> 
	      if List.mem hd except_fields then 
		acc 
	      else
		ZEquals (encode_header hd pkt1, encode_header hd pkt2)::acc) 
	    [] all_fields )

  let encode_vint (v: VInt.t): zTerm = TInt (VInt.get_int64 v)

(* some optimizations to make output more readable may be questionable form. *)
  let rec forwards (pol:policy) (pkt1:zVar) (pkt2: zVar): zFormula =
	let test_action hdr v pkt = ZEquals (encode_header hdr pkt, encode_vint v) in
    match pol with
      | Drop -> 
	ZFalse
      | Id -> 
		if pkt1 = pkt2 then ZTrue else 
		  ZEquals (TVar pkt1, TVar pkt2)
      | Test (hdr, v) -> 
		let hdr_test = test_action hdr v pkt1
		in
		if pkt1 = pkt2 then hdr_test else
		  ZAnd [hdr_test; ZEquals (TVar pkt1, TVar pkt2)]
      | Mod (hdr, v) -> 
	ZAnd [ZEquals (encode_header hdr pkt2, encode_vint v);
	      equal_field pkt1 pkt2 [hdr]]
      | Neg p ->
            (match p with
	      | Test (hdr, v) -> ZAnd [ ZNot (test_action hdr v pkt1); ZEquals (TVar pkt1, TVar pkt2) ]
	      | _ -> ZNot (forwards p pkt1 pkt2) )
      | Par (p1, p2) -> 
	ZOr [forwards p1 pkt1 pkt2;
	     forwards p2 pkt1 pkt2]
      | Seq (p1, p2) -> 
		(*I'm special-casing for debugging purposes *)
		(match p1, p2 with 
		  | Test (hdr, v), Test (hdr2, v2) -> ZAnd [ test_action hdr v pkt1;  test_action hdr2 v2 pkt1 ]
		  | Test (hdr, v), _ -> ZAnd [ test_action hdr v pkt1;  forwards p2 pkt1 pkt2]
		  | _, Test (hdr,v) -> ZAnd [ forwards p1 pkt1 pkt2; test_action hdr v pkt2]
		  | _ -> let pkt' = fresh SPacket in
					ZAnd [forwards p1 pkt1 pkt';
						  forwards p2 pkt' pkt2] )
	  | Star p1 -> failwith "NetKAT program not in form (p;t)*"


  let rec forwards_star (k:int) (Star (Seq (pol, topo))) (pkt1:zVar) (pkt2:zVar) : zFormula = 
    if k = 0 then 
      ZEquals (TVar pkt1, TVar pkt2)
    else
      let pkt' = fresh SPacket in 
      let pkt'' = fresh SPacket in 
      ZOr [ ZAnd [ forwards pol pkt1 pkt';
                   forwards topo pkt' pkt'';
                   forwards_star (k-1) (Star (Seq (pol, topo))) pkt'' pkt2 ];
            forwards_star (k-1) (Star (Seq (pol, topo))) pkt1 pkt2 ]
end

(* str: name of your test (unique ID)  
   inp: initial packet
   pol: policy to test
   outp: fully-transformed packet (megatron!)
   oko: optionof bool.  has to be Some.  True if you think it should be satisfiable.
*)
let check str inp p_t_star outp (oko : bool option) : bool = 
  Sat.fresh_cell := []; 
  let x = Sat.fresh Sat.SPacket in 
  let y = Sat.fresh Sat.SPacket in 
  let graph = NetKAT_Graph.parse_graph p_t_star in
  let prog = 
    Sat.ZProgram [ Sat.ZAssertDeclare (Verify.forwards inp x x)
                 ; Sat.ZAssertDeclare (Verify.forwards_star 1 p_t_star x y )
                 ; Sat.ZAssertDeclare (Verify.forwards outp y y) ] in 
  match oko, Sat.solve prog with 
  | Some ok, sat -> 
    if ok = sat then 
      true
    else
      (Printf.printf "[Verify.check %s: expected %b got %b]\n%!" str ok sat; false)
  | None, sat -> 
    (Printf.printf "[Verify.check %s: %b]\n%!" str sat; false)

(*  let rec forwards_star (k:int) (Star (Seq (pol, topo)) (pkt1:zVar) (pkt2:zVar) : zFormula =  *)
