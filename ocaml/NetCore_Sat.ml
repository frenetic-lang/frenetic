(* Switch, Port, DlDst, DlSrc *)
type zPacket = 
  ZPacket of Int64.t * int * Int64.t * Int64.t

type zVar = 
  string

type zSort = 
| SPacket
| SInt
| SFunction of zSort * zSort
| SRelation of zSort list

type zTerm = 
| TVar of zVar
| TPacket of zPacket
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
  | SFunction(sort1,sort2) -> 
    Printf.sprintf "(%s) %s" (serialize_sort sort1) (serialize_sort sort2)
  | SRelation(sorts) -> 
    Printf.sprintf "(%s)"
      (intercalate serialize_sort " " sorts)
    
let rec serialize_term term = match term with
  | TVar v -> v
  | TPacket pkt -> serialize_packet pkt
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
  "(declare-rel Switch (Packet Int))
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
  let postamble =      
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
