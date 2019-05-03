(** A packet is a map from fields to values. *)

open Core

module Field : sig
  type t = Fdd.Field.t [@@deriving compare]
  include Comparator.S with type t := t
end

module T : sig
  type t = int64 Map.M(Field).t
    [@@deriving sexp, compare, hash]
  include Comparator.S with type t := t
end

include T

val apply_action_seq : t -> Fdd.Value.t Fdd.Action.Seq.t -> t
val apply_action : t -> Fdd.Action.t -> Set.M(T).t
