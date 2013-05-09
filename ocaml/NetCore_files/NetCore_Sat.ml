(* Switch, Port, DlDst, DlSrc *)
type zPacket = 
  ZPacket of Int64.t * int * Int64.t * Int64.t

type zVar = 
  string

type zSort = 
| SPacket
| SInt
| SPath
| SFunction of zSort * zSort
| SRelation of zSort list

type zTerm = 
| TVar of zVar
| TPacket of zPacket
| TPath of zVar * zVar list
| TInt of Int64.t
| TFunction of zVar * zTerm list

type zAtom =
| ZTrue
| ZFalse 
| ZNot of zAtom
| ZEquals of zTerm * zTerm
| ZRelation of zVar * zTerm list
      
type zRule =
| ZRule of zVar * zVar list * zAtom list

type zDeclaration = 
| ZVarDeclare of zVar * zSort
| ZSortDeclare of zVar * (zVar * (zVar * zSort) list) list 
| ZFunDeclare of zVar * zVar * zSort * zSort * string

type zProgram = 
| ZProgram of zRule list * zVar
    
let init_decls : zDeclaration list = 
  [ ZSortDeclare("Packet", [("packet", [ ("PSwitch", SInt)
				       ; ("PInPort", SInt)
				       ; ("PDlSrc", SInt)
				       ; ("PDlDst", SInt) ])])]
(* Variables *)
let fresh_cell = ref []

let fresh sort = 
  let l = !fresh_cell in  
  let n = List.length l in 
  let x = match sort with
    | SPacket -> Printf.sprintf "_pkt%d" n 
    | SInt -> Printf.sprintf "_n%d" n
    | SPath -> Printf.sprintf "_path%d" n
    | SFunction _ -> Printf.sprintf "_f%d" n
    | SRelation _ -> Printf.sprintf "_R%d" n in 
  fresh_cell := ZVarDeclare(x,sort)::l;
  x

(* Serialization *)
let intercalate f s l = match l with 
  | [] -> 
    ""
  | h::t -> 
    List.fold_left (fun acc x -> acc ^ s ^ f x) (f h) t

let serialize_packet (ZPacket (switch, port, src, dst)) =
  Printf.sprintf "(Packet %s %d %s %s)" 
    (Int64.to_string switch) port 
    (Int64.to_string src) (Int64.to_string dst)

let rec serialize_sort sort = match sort with 
  | SPacket -> 
    "Packet"
  | SInt -> 
    "Int"
  | SPath ->
    "Path"
  | SFunction(sort1,sort2) -> 
    Printf.sprintf "(%s) %s" (serialize_sort sort1) (serialize_sort sort2)
  | SRelation(sorts) -> 
    Printf.sprintf "(%s)"
      (intercalate serialize_sort " " sorts)
    
let rec serialize_term term = match term with
  | TVar v -> v
  | TPacket pkt -> serialize_packet pkt
  | TPath (path, []) -> "nil"
  | TPath (path, pkts) ->
    List.fold_left
      (fun acc pkt ->
        let acc' = Printf.sprintf "(cons %s %s)" pkt acc in
        acc')
      path pkts
  | TInt n -> 
    Printf.sprintf "%s" (Int64.to_string n)
  | TFunction (f, terms) -> 
    Printf.sprintf "(%s %s)" f (intercalate serialize_term " " terms)

let rec serialize_atom atom = match atom with 
  | ZTrue -> 
    "true"
  | ZFalse -> 
    "false"
  | ZNot a1 -> 
    Printf.sprintf "(not %s)" (serialize_atom a1)
  | ZEquals (t1, t2) -> 
    Printf.sprintf "(equals %s %s)" (serialize_term t1) (serialize_term t2)
  | ZRelation(r, []) -> 
    Printf.sprintf "%s" r
  | ZRelation(r, terms) -> 
    Printf.sprintf "(%s %s)" r (intercalate serialize_term " " terms)

let serialize_atoms atoms = match atoms with 
  | [] -> 
    "true"
  | [atom] -> 
    serialize_atom atom
  | atom::rest -> 
    List.fold_right 
      (fun atom acc -> Printf.sprintf "(and %s %s)" acc (serialize_atom atom))
      rest (serialize_atom atom)

let serialize_rule rule = match rule with
  | ZRule(rel, [], atoms) -> 
    Printf.sprintf "(rule (=> %s %s))" 
      (serialize_atoms atoms) rel 
  | ZRule (rel, vars, atoms) -> 
    Printf.sprintf "(rule (=> %s (%s %s)))" 
      (serialize_atoms atoms) rel (intercalate (fun x -> x) " " vars)

let serialize_declaration declare = 
  match declare with
    | ZVarDeclare (x, sort) ->
      let decl = match sort with 
	| SFunction _ -> "fun"
	| SRelation _ -> "rel"
	| _ -> "var" in 
      Printf.sprintf "(declare-%s %s %s)" decl x (serialize_sort sort)
    | ZSortDeclare (name, constructorList) ->
      let serialize_field (field,sort) = 
	Printf.sprintf "(%s %s)" field (serialize_sort sort) in
      let serialize_constructor (name, fields) = 
	Printf.sprintf "(%s %s)" name (intercalate serialize_field " " fields) in 
      Printf.sprintf "(declare-datatypes () ((%s %s)))" 
	name (intercalate serialize_constructor " " constructorList)
    | ZFunDeclare (name, var, sort1, sort2, body) ->
      Printf.sprintf "(define-fun %s ((%s %s)) %s %s)" 
	name var (serialize_sort sort1) (serialize_sort sort2) body

let preamble =
  "(declare-datatypes () ((Link (mk-pair (sw Int) (pt Int)))))
   (declare-datatypes () ((Path (nil) (cons (hd Link) (tl Path)))))
   (declare-rel Switch (Packet Int))
   (declare-rel InPort (Packet Int))
   (declare-rel DlSrc (Packet Int))
   (declare-rel DlDst (Packet Int))
   (declare-var p Packet)
   (declare-var n Int)

   (rule (=> (and (= (PSwitch p) n) (>= n 0)) (Switch p n)))
   (rule (=> (and (= (PInPort p) n) (>= n 0)) (InPort p n)))
   (rule (=> (and (= (PDlSrc p) n) (>= n 0)) (DlSrc p n)))
   (rule (=> (and (= (PDlDst p) n) (>= n 0)) (DlDst p n)))"

let serialize_program (ZProgram (rules, query)) = 
(*  let postamble =      
    ":default-relation smt_relation2\n" ^ 
    ":engine pdr\n" ^
    ":print-answer true" in 
  Printf.sprintf 
    "%s\n%s\n%s\n%s\n(query %s\n%s)" 
    (intercalate serialize_declaration "\n" init_decls)
    (intercalate serialize_declaration "\n" (!fresh_cell))
    preamble
    (intercalate serialize_rule "\n" rules) 
    query
    postamble
*)
"(declare-datatypes () ((Packet (packet (PSwitch Int) (PInPort Int) (PDlSrc Int) (PDlDst Int)))))
(declare-datatypes () ((Path (nil) (cons (head Packet) (tail Path)))))
(declare-var _n16 Int)
(declare-var _n15 Int)
(declare-var _n14 Int)
(declare-var _n13 Int)
(declare-var _n12 Int)
(declare-var _n11 Int)
(declare-var _n10 Int)
(declare-var _pkt9 Packet)
(declare-var _pkt8 Packet)
(declare-var _pkt7 Packet)
(declare-var _pkt6 Packet)
(declare-rel _R5 (Packet Packet Path))
(declare-rel _R4 (Packet Packet))
(declare-rel _R3 (Packet Packet))
(declare-rel _R2 ())
(declare-var _pkt1 Packet)
(declare-var _pkt0 Packet)
(declare-datatypes () ((Link (mk-pair (sw Int) (pt Int)))))
   (declare-rel Switch (Packet Int))
   (declare-rel InPort (Packet Int))
   (declare-rel DlSrc (Packet Int))
   (declare-rel DlDst (Packet Int))
   (declare-var p Packet)
   (declare-var n Int)
   (declare-var path Path)

   (rule (=> (and (= (PSwitch p) n) (>= n 0)) (Switch p n)))
   (rule (=> (and (= (PInPort p) n) (>= n 0)) (InPort p n)))
   (rule (=> (and (= (PDlSrc p) n) (>= n 0)) (DlSrc p n)))
   (rule (=> (and (= (PDlDst p) n) (>= n 0)) (DlDst p n)))
(rule (=> (and (and (and (and (Switch _pkt0 1) (_R5 _pkt0 _pkt1 nil)) (InPort _pkt1 2)) (Switch _pkt1 3)) (InPort _pkt0 1)) _R2))
(rule (=> (and (and (and (and (and (and (and (and (and true (not (equals _n10 _n11))) (InPort _pkt7 _n11)) (InPort _pkt6 _n10)) (Switch _pkt7 _n12)) (Switch _pkt6 _n12)) (DlSrc _pkt7 _n13)) (DlSrc _pkt6 _n13)) (DlDst _pkt7 _n14)) (DlDst _pkt6 _n14)) (_R3 _pkt6 _pkt7)))
(rule (=> (and (and (and (and (and (and (and (DlDst _pkt6 _n16) (InPort _pkt7 2)) (Switch _pkt7 2)) (InPort _pkt6 3)) (Switch _pkt6 3)) (DlSrc _pkt7 _n15)) (DlSrc _pkt6 _n15)) (DlDst _pkt7 _n16)) (_R4 _pkt6 _pkt7)))
(rule (=> (and (and (and (and (and (and (and (DlDst _pkt6 _n16) (InPort _pkt7 3)) (Switch _pkt7 3)) (InPort _pkt6 2)) (Switch _pkt6 2)) (DlSrc _pkt7 _n15)) (DlSrc _pkt6 _n15)) (DlDst _pkt7 _n16)) (_R4 _pkt6 _pkt7)))
(rule (=> (and (and (and (and (and (and (and (DlDst _pkt6 _n16) (InPort _pkt7 1)) (Switch _pkt7 2)) (InPort _pkt6 3)) (Switch _pkt6 1)) (DlSrc _pkt7 _n15)) (DlSrc _pkt6 _n15)) (DlDst _pkt7 _n16)) (_R4 _pkt6 _pkt7)))
(rule (=> (and (and (and (and (and (and (and (DlDst _pkt6 _n16) (InPort _pkt7 3)) (Switch _pkt7 1)) (InPort _pkt6 1)) (Switch _pkt6 2)) (DlSrc _pkt7 _n15)) (DlSrc _pkt6 _n15)) (DlDst _pkt7 _n16)) (_R4 _pkt6 _pkt7)))
(rule (=> (and (and (and (and (and (and (and (DlDst _pkt6 _n16) (InPort _pkt7 1)) (Switch _pkt7 3)) (InPort _pkt6 4)) (Switch _pkt6 1)) (DlSrc _pkt7 _n15)) (DlSrc _pkt6 _n15)) (DlDst _pkt7 _n16)) (_R4 _pkt6 _pkt7)))
(rule (=> (and (and (and (and (and (and (and (DlDst _pkt6 _n16) (InPort _pkt7 4)) (Switch _pkt7 1)) (InPort _pkt6 1)) (Switch _pkt6 3)) (DlSrc _pkt7 _n15)) (DlSrc _pkt6 _n15)) (DlDst _pkt7 _n16)) (_R4 _pkt6 _pkt7)))
(rule (=> (_R3 _pkt6 _pkt7) (_R5 _pkt6 _pkt7 path)))
(rule (=> (and (and (_R3 _pkt6 _pkt8) (_R5 _pkt9 _pkt7 (cons _pkt7 (cons _pkt9 (cons _pkt8 (cons _pkt6 path)))))) (_R4 _pkt8 _pkt9)) (_R5 _pkt6 _pkt7 path)))
(query _R2
:default-relation smt_relation2
:engine pdr
:print-answer true)"



let solve prog = 
  let s = serialize_program prog in 
  let _ = Misc.Log.printf "--- DEBUG ---\n%s\n%!" s in 
  let ch = open_out ".z3.in" in 
  let _ = output_string ch s in 
  let _ = flush ch in 
  let _ = close_out ch in 
  let _ = Sys.command "z3 -smt2 -nw .z3.in > .z3.out" in 
  let ch = open_in ".z3.out" in 
  let bs = in_channel_length ch in 
  let r = String.create bs in 
  let _ = really_input ch r 0 bs in 
  r

module Topology = NetCore_Sat_Topology
