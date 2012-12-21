module type WORD = sig

  type t

  val eq_dec : t -> t -> bool

  val test_bit : int -> t -> bool
  val clear_bit : int -> t -> t
  val set_bit : int -> t -> t
  val bit : t -> int -> bool -> t
end

module Word8 : sig
  include WORD 
  val from_int : int -> t
  val to_int : t -> int
end

module Word16 : sig
  type t = int
  val eq_dec : t -> t -> bool
  val test_bit : int -> t -> bool
  val clear_bit : int -> t -> t
  val set_bit : int -> t -> t
  val bit : t -> int -> bool -> t
  val from_int : int -> t
  val to_int : t -> int
end

module Word32 : sig
  include WORD
  val to_int32 : t -> Int32.t
  val from_int32 : Int32.t -> t
end

module Word48 : sig
  include WORD
  val to_bytes : t -> string
  val from_bytes : string -> t
end

module Word64 : sig
  include WORD
  val to_int64 : t -> Int64.t
  val from_int64 : Int64.t -> t
end
