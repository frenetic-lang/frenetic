(** Types representing Gurobi LP file format *)

exception LPParseError of string

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

type const =
  | Constraint of expr * rel * int64
  | Indicator  of string * bool * expr * rel * int64

type constraints = const list

type bound = Bound of expr * rel * expr

type bounds = bound list

type objective =
  | Maximize of expr list
  | Minimize of expr list

type t = LP of objective * constraints * bounds * types * sos

val sum : expr list -> expr

val to_string : t -> string

val clean : string -> unit
val write : string -> string -> unit
val read : string -> string list
