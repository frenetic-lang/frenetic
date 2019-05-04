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

include T with type t = int64 Map.M(Field).t

(** the packet with no fields *)
val empty : t

val apply_fdd : Fdd.FDD.t -> t -> Fdd.Action.t
val apply_action : Fdd.Action.t -> t -> Set.M(T).t
val apply_action_seq : Fdd.Value.t Fdd.Action.Seq.t -> t -> t

val eval_e_fdd : Fdd.FDD.t -> t -> Set.M(T).t
val eval_d_fdd : Fdd.FDD.t -> t -> int64 Map.M(T).t
