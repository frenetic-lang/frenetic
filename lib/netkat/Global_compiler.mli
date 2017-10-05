open Syntax
open Local_compiler

(** Intermediate representation of global compiler: NetKAT Automata *)
module Automaton : sig
  type t

  val fold_reachable: ?order:[< `Post | `Pre > `Pre ]
    -> t
    -> init:'a
    -> f:('a -> int64 -> (FDD.t * FDD.t) -> 'a)
    -> 'a

  val of_policy: ?dedup:bool -> ?ing:pred -> ?cheap_minimize:bool -> policy -> t
  val to_local: pc:Field.t -> t -> FDD.t

  val to_dot: t -> string
end

val compile : ?options:compiler_options -> ?pc:Field.t -> policy -> t
(** [compile p] returns the intermediate representation of the global
    policy [p]. The pc field is used for internal bookkeeping and must *not* be
    accessed or written to by the input policy [p].

    You can generate a flowtable from [t] by passing it to the {!to_table}
    function below.
 *)
