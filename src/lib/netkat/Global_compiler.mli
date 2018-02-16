open Syntax
open Core

module FDD : module type of Local_compiler.FDD

(** Intermediate representation of global compiler: NetKAT Automata *)
module Automaton : sig
  type t = private {
    states : (int64, FDD.t * FDD.t) Hashtbl.t;
    has_state : (FDD.t * FDD.t, int64) Hashtbl.t;
    mutable source : int64;
    mutable nextState : int64
  }

  val create_t : unit -> t

  val add_to_t_with_id : t -> (FDD.t * FDD.t) -> int64 -> unit

  val iter_reachable: ?order:[`Post | `Pre]
    -> t
    -> f:(int64 -> (FDD.t * FDD.t) -> unit)
    -> unit

  val fold_reachable: ?order:[`Post | `Pre]
    -> t
    -> init:'a
    -> f:('a -> int64 -> (FDD.t * FDD.t) -> 'a)
    -> 'a

  val of_policy: ?dedup:bool -> ?ing:pred -> ?cheap_minimize:bool -> policy -> t
  val to_local: pc:Fdd.Field.t -> t -> FDD.t

  val to_dot: t -> string

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
