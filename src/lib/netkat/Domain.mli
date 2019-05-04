(** The domain of an FDD/Automaton is given by the set of values occuring with
    each field, either in a test or a modification.
 *)

open Core

module Field : sig
  type t = Fdd.Field.t [@@deriving compare]
  include Comparator.S with type t := t
end

type t = Set.M(Int64).t Map.M(Field).t  (** for each field, a set of values *)

val empty : t

val merge : t -> t -> t

val of_fdd : Fdd.FDD.t -> t

val of_automaton : Global_compiler.Automaton.t -> t

val representative_pks : t -> Packet.t list
