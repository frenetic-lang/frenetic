module type HASHABLE = sig
  type t
  val hash : t -> int
  val equal : t -> t -> bool
end

module type HASHTYPE = sig
  type value
  val get : value -> int
  val unget : int -> value
  val clear : Core.Std.Int.Set.t -> unit
end

module Make (Value : HASHABLE) : HASHTYPE with type value = Value.t