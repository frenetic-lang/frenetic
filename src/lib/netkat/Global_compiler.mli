open Syntax
open Core

module FDD : module type of Local_compiler.FDD

(** Intermediate representation of global compiler: NetKAT Automata *)
module Automaton : sig
  type t = private
    { states : (int64, FDD.t * FDD.t) Hashtbl.t;
      has_state : (FDD.t * FDD.t, int64) Hashtbl.t;
      mutable source : int64;
      mutable nextState : int64 }

  val add_to_t : t -> (FDD.t * FDD.t) -> int64

  val fold_reachable: ?order:[< `Post | `Pre > `Pre ]
    -> t
    -> init:'a
    -> f:('a -> int64 -> (FDD.t * FDD.t) -> 'a)
    -> 'a

  val of_policy: ?dedup:bool -> ?ing:pred -> ?cheap_minimize:bool -> policy -> t
  val to_local: pc:Fdd.Field.t -> t -> FDD.t

  val to_dot: t -> string

  val render : ?format:string -> ?title:string -> t -> unit
  (** Compiles the provided automaton `t` using `graphviz`, and opens the resulting
      file. *)

  val skip_topo_states : t
    -> ((int64, (int64 * int64)) Hashtbl.t * ((int64 * int64), Int64.Set.t) Hashtbl.t)

end


val compile : ?options:Local_compiler.compiler_options
           -> ?pc:Fdd.Field.t
           -> ?ing:pred
           -> policy
           -> FDD.t
(** [compile p] compiles the policy [p] into an FDD. The pc field is used for
    internal bookkeeping and must *not* be accessed or written to by the input
    policy [p].
*)
