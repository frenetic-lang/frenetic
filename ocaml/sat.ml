let sprintf = Printf.sprintf

type z3Packet = Z3Packet of string

type z3Int = Z3Int of string

type const = 
  | ConstPacket of z3Packet
  | ConstInt of z3Int

module Constants = struct
  type t = string
  let compare = Pervasives.compare
end

module S = Set.Make(Constants)

let write_const c =
  match c with
    | ConstPacket (Z3Packet p) -> sprintf "%s Packet" p
    | ConstInt (Z3Int i) -> sprintf "%s Int" i

type intExp = 
  | Primitive of Int64.t
  | PktHeader of string * z3Packet
  | Variable of z3Int

let write_intExp i = 
  match i with
    | Primitive i1 -> Int64.to_string i1
    | PktHeader (h, (Z3Packet  p)) -> sprintf "(%s %s)" h p
    | Variable (Z3Int s) -> s

type boolExp = 
  | ZTrue
  | ZFalse 
  | ZNot of boolExp
  | ZAnd of boolExp * boolExp
  | ZOr of boolExp * boolExp
  | Implies of boolExp * boolExp
  | Equals of intExp * intExp
  | ForAll of const list * boolExp
  | Exists of const list * boolExp

let write_constList l =
  match l with
    | [] -> "";
    | _ -> List.fold_left (fun constList const -> (sprintf "(%s) " (write_const const)) ^ constList) 
      ("(" ^ write_const (List.hd l) ^ ")") (List.tl l)

let rec serialize_boolExp b = 
  match b with 
    | ZTrue -> "true"
    | ZFalse -> "false"
    | ZNot b1 -> sprintf "(not %s)" (serialize_boolExp b1)
    | ZAnd(b1,b2) -> sprintf "(and %s %s)" (serialize_boolExp b1) (serialize_boolExp b2)
    | ZOr (b1, b2) -> sprintf "(or %s %s)" (serialize_boolExp b1) (serialize_boolExp b2)
    | Implies (b1, b2) -> sprintf "(implies %s %s)" (serialize_boolExp b1) (serialize_boolExp b2)
    | Equals (i1, i2) -> sprintf "(equals %s %s)" (write_intExp i1) (write_intExp i2)
    | ForAll (c, b1) -> sprintf "(forall (%s) %s)" (write_constList c) (serialize_boolExp b1)
    | Exists (c, b1) -> sprintf "(exists (%s) %s)" (write_constList c) (serialize_boolExp b1)

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

let z3_postlude = 
  "(check-sat)
   (get-model)"

let rec constants_of_boolExp b s = 
  match b with
    | Equals (i1, i2) ->
      let s1 =
	begin
	  match i1 with
	    | PktHeader (str, Z3Packet p) -> S.add (sprintf "%s Packet" p) s
	    | Variable (Z3Int i) -> S.add (sprintf "%s Int" i) s
	    | _ -> s
	end
      in
      begin
        match i2 with
	  | PktHeader (str, Z3Packet p) -> S.add (sprintf "%s Packet" p) s1
	  | Variable (Z3Int i) -> S.add (sprintf "%s Int" i) s1
	  | _ -> s
      end
    | ForAll (constList, b1) -> 
      let s1 = List.fold_left (fun x y -> S.add (write_const y) x) s constList in
      constants_of_boolExp b1 s1
    | Exists (constList, b1) -> 
      let s1 = List.fold_left (fun x y -> S.add (write_const y) x) s constList in
      constants_of_boolExp b1 s1
    | ZNot b1 -> constants_of_boolExp b1 s 
    | ZAnd (b1, b2) | ZOr (b1, b2) | Implies (b1, b2) -> let s1 = constants_of_boolExp b1 s in
							 constants_of_boolExp b2 s1
    | ZTrue | ZFalse -> s

let declare_constants s =
  S.fold (fun x y -> (sprintf "(declare-const %s)\n" x) ^ y) s ""


let solve b = 
  let c = constants_of_boolExp b S.empty in
  let s = 
    sprintf "%s\n%s\n(assert %s)\n%s"
      z3_prelude
      (declare_constants c)
      (serialize_boolExp b)
      z3_postlude in 
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
