(** Types representing Gurobi LP file format *)

type sos = Sos of string | NoSos

type rel =
  | Eq
  | Leq
  | Geq

type type_decl =
  | Binary   of string list
  | Integers of string list
  | Semis    of string list
  | Generals of string list

type types =
  type_decl list

type expr =
  | Var   of string
  | Float of float
  | Int   of int64
  | Plus  of expr * expr
  | Minus of expr * expr
  | Mult  of expr * expr
  | Div   of expr * expr

type const = Constraint of expr * rel * expr

type constraints = const list

type bound = Bound of expr * rel * expr

type bounds = bound list

type objective =
  | Maximize of expr list
  | Minimize of expr list

type t = LP of objective * constraints * bounds * types * sos

module Buf = struct
  open Buffer
  let rec of_linear_expr e buf =
    match e with
    | Var(s) -> add_string buf s
    | Float(f) -> add_string buf (Printf.sprintf "%.1f" f)
    | Int(i)-> add_string buf (Int64.to_string i)
    | Plus(lhs,rhs) ->
      of_linear_expr lhs buf;
      add_string buf " + ";
      of_linear_expr rhs buf
    | Minus(lhs,rhs) ->
      of_linear_expr lhs buf;
      add_string buf " - ";
      of_linear_expr rhs buf
    | Mult(lhs,rhs) ->
      of_linear_expr lhs buf;
      add_string buf " ";
      of_linear_expr rhs buf
    | Div(lhs,rhs) ->
      of_linear_expr lhs buf;
      add_string buf " / ";
      of_linear_expr rhs buf

  let of_relop o buf =
    match o with
    | Eq -> add_string buf "="
    | Leq -> add_string buf "<="
    | Geq -> add_string buf ">="

  let of_const c buf =
    match c with
    | Constraint(x,y,z) ->
      of_linear_expr x buf;
      add_string buf "\n";
      of_relop y buf;
      add_string buf " ";
      of_linear_expr z buf

  let of_bound c buf =
    match c with
    | Bound(x,y,z) ->
      of_linear_expr x buf;
      add_string buf " ";
      of_relop y buf;
      add_string buf " ";
      of_linear_expr z buf

  let of_id_pw ls buf =
    List.iter (fun l -> add_string buf l ; add_string buf " ") ls

  let of_const_pn ls buf =
    List.iter (fun l -> of_const l buf; add_string buf "\n") ls

  let of_bound_pn bs buf =
    List.iter (fun l -> of_bound l buf; add_string buf "\n") bs

  let of_sos s buf =
    match s with
    | Sos(str) -> add_string buf str
    | NoSos -> ()

  let of_type_decl t buf =
    match t with
    | Binary(l) ->
      add_string buf "Binary\n";
      of_id_pw l buf
    | Integers(l) ->
      add_string buf "Integers\n    %s";
      of_id_pw l buf
    | Semis(l) ->
      add_string buf "Semis\n    %s";
      of_id_pw l buf
    | Generals(l) ->
      add_string buf "Generals\n    %s";
      of_id_pw l buf

  let of_types ts buf =
    List.iter (fun t -> (of_type_decl t buf); add_string buf "\n") ts

  let of_bounds b buf =
    add_string buf "Bounds\n";
    of_bound_pn b buf

  let of_constraints l buf =
    add_string buf "Subject To\n";
    (of_const_pn l buf)

  let of_objective o buf =
    match o with
    | Maximize(ls) ->
      add_string buf "Maximize\n";
      List.iter (fun l -> of_linear_expr l buf ; add_string buf "\n") ls
    | Minimize(ls) ->
      add_string buf "Minimize\n";
      List.iter (fun l -> of_linear_expr l buf ; add_string buf "\n") ls

  let of_lp lp =
    let buf = create 1000 in
    let LP(o, c, b, t, s) = lp in
    of_objective o buf;
    of_constraints c buf;
    of_bounds b buf;
    of_types t buf;
    add_string buf "\nEnd";
    buf
end

let to_string lp = Buffer.contents ( Buf.of_lp lp )
