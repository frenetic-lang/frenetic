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

val to_string : t -> string
