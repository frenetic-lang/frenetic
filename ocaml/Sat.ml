type z3Packet = 
  string

type zVar = 
  string

type zSort = 
| SPacket
| SInt
| SFunction of zSort * zSort
| SRelation of zSort list

type zTerm = 
| TVar of zVar
| TPacket of z3Packet
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
| ZDeclare of zVar * zSort 

type zProgram = 
| ZProgram of zRule list * zVar
    
let init_decls : zDeclaration list = 
  [ ZDeclare("DlSrc", SFunction(SPacket,SInt))
  ; ZDeclare("DlDst", SFunction(SPacket,SInt))
  ; ZDeclare("SrcIP", SFunction(SPacket,SInt))
  ; ZDeclare("DstIP", SFunction(SPacket,SInt))
  ; ZDeclare("TcpSrcPort", SFunction(SPacket,SInt))
  ; ZDeclare("TcpDstPort", SFunction(SPacket,SInt))
  ; ZDeclare("InPort", SFunction(SPacket,SInt))
  ; ZDeclare("Switch", SFunction(SPacket,SInt))
  ; ZDeclare("Forwards", SRelation([SPacket; SPacket])) ]

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
  fresh_cell := ZDeclare(x,sort)::l;
  x

(* Serialization *)
let intercalate f s l = match l with 
  | [] -> 
    ""
  | h::t -> 
    List.fold_right (fun x acc -> acc ^ s ^ f x) t (f h)

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
  | TPacket pkt -> pkt
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
  | ZRelation (r, terms) -> 
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

let serialize_rule (ZRule (rel, vars, atoms)) =
  Printf.sprintf "(rule (=> %s (%s %s)))" 
    (serialize_atoms atoms) rel (intercalate (fun x -> x) " " vars)

let serialize_declaration (ZDeclare (x,sort)) = 
  let decl = match sort with 
    | SFunction _ -> "fun"
    | SRelation _ -> "rel"
    | _ -> "var" in 
  Printf.sprintf "(declare-%s %s %s)" decl x (serialize_sort sort)

let serialize_program (ZProgram (rules, query)) =
  let preamble = 
    "(declare-sort Packet)" in 
  let postamble =      
    ":default-relation smt_relation2\n" ^ 
    ":engine datalog\n" ^
    ":print-answer true" in 
  Printf.sprintf 
    "%s\n%s\n%s\n%s\n(query %s\n%s)" 
    preamble
    (intercalate serialize_declaration "\n" init_decls)
    (intercalate serialize_declaration "\n" (!fresh_cell))
    (intercalate serialize_rule "\n" rules) 
    query
    postamble

let solve prog = 
  let s = serialize_program prog in 
  let _ = Printf.eprintf "--- DEBUG ---\n%s\n%!" s in 
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
