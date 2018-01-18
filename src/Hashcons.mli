open Core

module type HASHTYPE = sig
  type t
  val get : t -> int
  val unget : int -> t
  val clear : Int.Set.t -> unit
  val cache_size : unit -> int
end

module Make (Value : Hashtbl.Key) : HASHTYPE with type t = Value.t
