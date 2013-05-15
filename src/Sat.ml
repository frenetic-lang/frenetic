type zPacket = 
  ZPacket of Int64.t * int * Int64.t * Int64.t

type zVar = 
  string

type zSort = 
| SPacket
| SInt
| SPath
| SPair
| SFunction of zSort * zSort
| SRelation of zSort list

type zTerm = 
| TVar of zVar
| TInt of Int64.t
| TPacket of zPacket
| TPath of zTerm list
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
    
(* Variables *)
let fresh_cell = ref []

let fresh sort = 
  let l = !fresh_cell in  
  let n = List.length l in 
  let x = match sort with
    | SPacket -> Printf.sprintf "_pkt%d" n 
    | SInt -> Printf.sprintf "_n%d" n
    | SPair -> Printf.sprintf "_pair%d" n 
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
    (Int64.to_string switch) 
    port 
    (Int64.to_string src) 
    (Int64.to_string dst)

let rec serialize_sort = function
  | SPacket -> 
    "Packet"
  | SInt -> 
    "Int"
  | SPair ->
    "Pair"
  | SPath ->
    "Path"
  | SFunction(sort1,sort2) -> 
    Printf.sprintf "(%s) %s" (serialize_sort sort1) (serialize_sort sort2)
  | SRelation(sorts) -> 
    Printf.sprintf "(%s)"
      (intercalate serialize_sort " " sorts)
    
let rec serialize_term = function 
  | TVar x -> 
    x
  | TPacket pkt -> 
    serialize_packet pkt
  | TPath ([]) ->
    "(nil)"
  | TPath(pkt::pth) -> 
    Printf.sprintf 
      "(cons (pair (Switch %s) (Inport %s)) %s)" 
      (serialize_term pkt)
      (serialize_term pkt)
      (serialize_term (TPath(pth)))
  | TInt n -> 
    Printf.sprintf "%s" (Int64.to_string n)
  | TFunction (f, terms) -> 
    Printf.sprintf "(%s %s)" f (intercalate serialize_term " " terms)

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
  | ZAssertDeclare(f) -> 
    Printf.sprintf "(assert %s)" (serialize_formula f)

let init_decls : zDeclaration list = 
  [ ZSortDeclare
      ("Packet", [("packet", [ ("PSwitch", SInt)
		             ; ("PInPort", SInt)
		             ; ("PDlSrc", SInt)
		             ; ("PDlDst", SInt) ])])
  ; ZSortDeclare
       ("Pair", [("pair", [("First", SInt)
                             ;("Second", SInt)])])
  ;  ZSortDeclare
    ("Path", [("nil", []);
              ("cons", [("Head", SPair);
                        ("Tail", SPair)])])
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
  (* let _ = Misc.Log.printf "--- DEBUG ---\n%s\n%!" s in  *)
  let ch = open_out ".z3.in" in 
  let _ = output_string ch s in 
  let _ = flush ch in 
  let _ = close_out ch in 
  let _ = Sys.command "z3 -smt2 -nw .z3.in > .z3.out" in 
  let ch = open_in ".z3.out" in 
  let bs = in_channel_length ch in 
  let r = String.create bs in 
  let _ = really_input ch r 0 bs in 
  r = "sat\n"
