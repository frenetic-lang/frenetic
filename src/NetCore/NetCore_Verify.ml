open Syntax.External

module Sat = struct
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
end

(* Topology *)
type link = Link of OpenFlow0x01.Types.switchId * Packet.Types.portId

type topology = Topology of (link * link) list

let reverse_edge (sp1, sp2) = (sp2, sp1)

let bidirectionalize (Topology topo) = 
  Topology 
    (List.fold_left 
       (fun acc edge -> edge::(reverse_edge edge)::acc) 
       [] topo)

let packet_field (field:string) (pkt:zVar) (num:zTerm) : zFormula = 
  ZEquals(TFunction(field, [TVar pkt]), num)

(* Predicates *)
let rec encode_predicate (pred:predicate) (pkt:zVar) : zFormula =
  match pred with
  | All -> 
    ZTrue
  | NoPackets ->
    ZFalse
  | Not pred1 ->
    ZNot(encode_predicate pred1 pkt)
  | And (pred1, pred2) ->
    ZAnd([encode_predicate pred1 pkt;
          encode_predicate pred2 pkt])
  | Or(pred1, pred2) -> 
    ZOr([encode_predicate pred1 pkt;
          encode_predicate pred2 pkt])
  | DlSrc mac -> 
    packet_field "DlSrc" pkt (TInt mac)
  | DlDst mac -> 
    packet_field "DlSrc" pkt (TInt mac)
  | DlVlan (Some vlan) -> 
    packet_field "DlVlan" pkt (TInt (Int64.of_int vlan))
  | DlVlan None -> 
    failwith "Not yet implemented"
  | SrcIP ip -> 
    packet_field "SrcIP" pkt (TInt (Int64.of_int32 ip))
  | DstIP ip -> 
    packet_field "DstIP" pkt (TInt (Int64.of_int32 ip))
  | TcpSrcPort port -> 
    packet_field "TcpSrcPort" pkt (TInt (Int64.of_int port))
  | TcpDstPort port -> 
    packet_field "TcpDstPort" pkt (TInt (Int64.of_int port))
  | InPort portId -> 
    packet_field "InPort" pkt (TInt (Int64.of_int portId))
  | Switch switchId -> 
    packet_field "Switch" pkt (TInt switchId)
          
let equal_field (field:string) (pkt1:zVar) (pkt2:zVar) : zFormula list = 
  let num = TVar (fresh SInt) in 
  [packet_field field pkt1 num; packet_field field pkt2 num]

let equals (fields:string list) (pkt1:zVar) (pkt2:zVar) : zFormula list = 
  List.fold_left (fun acc field -> equal_field field pkt1 pkt2 @ acc) [] fields

let action_forwards (act:action) (pkt1:zVar) (pkt2:zVar) : zFormula = 
  match act with 
  | To pId ->
    ZAnd(equals [ "Switch"; "DlSrc"; "DlDst" ] pkt1 pkt2 @ 
         [packet_field "InPort" pkt2 (TInt (Int64.of_int pId))])
  | ToAll ->
    let num1 = TVar (fresh SInt) in
    let num2 = TVar (fresh SInt) in
    ZAnd(equals [ "Switch"; "DlSrc"; "DlDst" ] pkt1 pkt2 @
           [packet_field "InPort" pkt1 num1;
            packet_field "InPort" pkt2 num2;
            ZNot (ZEquals (num1, num2))])
  | GetPacket gph ->
    ZFalse
  | _ -> 
    failwith "action_forwards: not yet implemented"

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

let rec policy_forwards (pol:policy) (pkt1:zVar) (pkt2:zVar) : zFormula = 
  match pol with
  | Act (action) -> 
    action_forwards action pkt1 pkt2
  | Par (pol1, pol2) ->
    ZOr([ policy_forwards pol1 pkt1 pkt2
        ; policy_forwards pol2 pkt1 pkt2 ])
  | Seq (pol1, pol2) ->
    let pkt' = fresh SPacket in 
    ZAnd([policy_forwards pol1 pkt1 pkt';
          policy_forwards pol2 pkt' pkt2])
  | Filter pred ->
    let eq = equals ["Switch"; "InPort"; "DlSrc"; "DlDst"] pkt1 pkt2 in 
    ZAnd(encode_predicate pred pkt1::eq)
  | Empty -> 
    ZFalse
  | _ -> 
    failwith "policy_forwards: not yet implemented"

let rec forwards (k:int) (topo:topology) (pol:policy) (pkt1:zVar) (pkt2:zVar) : zFormula = 
  if k = 0 then 
    ZAnd(equals ["Switch"; "InPort"; "DlSrc"; "DlDst"] pkt1 pkt2)
  else
    let pkt' = fresh SPacket in 
    let pkt'' = fresh SPacket in 
    ZOr [ ZAnd [ policy_forwards pol pkt1 pkt'
               ; topology_forwards topo pkt' pkt''
               ; forwards (k-1) topo pol pkt'' pkt2 ]
        ; forwards (k-1) topo pol pkt1 pkt2 ]

type example = { name:string;
                 topology:topology;
                 policy:policy;
                 formula:topology -> policy -> zVar -> zVar -> zFormula;
                 expected:bool }

let check ex = 
  let pkt1 = fresh SPacket in 
  let pkt2 = fresh SPacket in 
  let f = ex.formula ex.topology ex.policy pkt1 pkt2 in 
  let p = ZProgram [ZAssertDeclare f] in 
  if solve p = ex.expected then 
    Printf.eprintf "%s [[32mpassed[0m]\n" ex.name
  else 
    Printf.eprintf "%s [[31mfailed[0m]\n" ex.name

let ex1 : example = 
  let topology = 
    let s1 = Int64.of_int 1 in
    let s2 = Int64.of_int 2 in
    let s3 = Int64.of_int 3 in
    bidirectionalize 
      (Topology [ (Link (s1, 2), Link (s2, 1)) 
                ; (Link (s2, 2), Link (s3, 1)) ]) in 
  let policy = Act ToAll in
  let formula = forwards 2 in 
  { name="Linear";
    topology=topology;
    policy=policy;
    formula=formula;
    expected=true }

let examples = [ex1]
    
let () = List.iter check examples
