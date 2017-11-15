open Core

module type HASHTYPE = sig
  type t
  val get : t -> int
  val unget : int -> t
  val clear : Int.Set.t -> unit
end

module Make (Value : Hashtbl.Key) : HASHTYPE with type t = Value.t
