let sprintf = Printf.sprintf

type z3Packet = string

type zVar = string

type zTerm = 
  | Var of zVar
  | Packet of z3Packet
  | Int of int
  | Func of string * zTerm list

type zRelName = string

type zAtom =
  | ZTrue
  | ZFalse 
  | ZNot of zAtom
  | ZEquals of zTerm * zTerm
  | ZRel of zRelName * zTerm list

type zRule =
  ZRule of zRelName * zVar list * zAtom list

type zProgram = 
   ZProgram of zRule list * zRelName

let intercalate l s f =
  match l with
    | [] -> ""
    | h::t -> List.fold_right (fun x acc -> acc ^ s ^ f x) t (f h)

let rec serialize_term t = 
  match t with
    | Var v -> v
    | Packet p -> p
    | Int i -> sprintf "%d" i
    | Func (s, tList) -> sprintf "(%s %s)" s (intercalate tList " " serialize_term)

let rec serialize_atom a = 
  match a with 
    | ZTrue -> "true"
    | ZFalse -> "false"
    | ZNot a1 -> sprintf "(not %s)" (serialize_atom a1)
    | ZEquals (t1, t2) -> sprintf "(equals %s %s)" (serialize_term t1) (serialize_term t2)
    | ZRel (name, tList) -> sprintf "(%s %s)" name (intercalate tList " " serialize_term)

let datalogStuff =
  ":default-relation smt_relation2\n:engine datalog\n:print-answer true"

let andAtoms aList =
match aList with 
  | [] -> "true"
  | h::[] -> serialize_atom h
  | h::t -> List.fold_right (fun a acc -> sprintf "(and %s %s)" acc (serialize_atom a)) t (serialize_atom h)

let serialize_rule (ZRule (r, xList, aList)) =
  sprintf "(rule (=> %s (%s %s)))" (andAtoms aList) r (intercalate xList " " (fun x -> x))

let serialize_program (ZProgram (rList, name)) =
  sprintf "%s\n(query %s\n%s)" 
    (intercalate rList "\n" serialize_rule) name (datalogStuff)
  

let z3_prelude =
  "(declare-sort Packet)
   (declare-fun DlSrc (Packet) Int)
   (declare-fun DlDst (Packet) Int)
   (declare-fun DlTyp (Packet) Int)
   (declare-fun DlVlan (Packet) Int)
   (declare-fun DlVlanPcp (Packet) Int)
   (declare-fun NwSrc (Packet) Int)
   (declare-fun NwDst (Packet) Int)
   (declare-fun NwProto (Packet) Int)
   (declare-fun NwTos (Packet) Int)
   (declare-fun TpSrc (Packet) Int)
   (declare-fun TpDst (Packet) Int)
   (declare-fun InPort (Packet) Int)
   (declare-fun Switch (Packet) Int)"

(* Print variables, packets *)
let serialize_declarations  (ZProgram (rList, name)) = ""


let solve prog = 
  let s = 
    sprintf "%s\n%s\n%s"
      z3_prelude
      (serialize_declarations prog)
      (serialize_program prog) in 
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
