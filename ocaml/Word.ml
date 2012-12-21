module type WORD = sig

  type t

  val eq_dec : t -> t -> bool

  val test_bit : int -> t -> bool
  val clear_bit : int -> t -> t
  val set_bit : int -> t -> t
  val bit : t -> int -> bool -> t

end

module Word8 = struct

  type t = char

  let eq_dec x y = x = y

  let test_bit n x = ((Char.code x) lsr n) land 1 = 1
  let clear_bit n x = Char.chr (Char.code x land (lnot (1 lsl n)))
  let set_bit n x = Char.chr (Char.code x lor (1 lsl n))
  let bit (x : t) (n : int) (v : bool) : t = 
    if v then set_bit n x else clear_bit n x

  let from_int (n : int) : t = Char.chr n
  let to_int (x : t) : int = Char.code x
end
  
module Word16  = struct

  type t = int

  let eq_dec x y = x = y

  let test_bit n x = (x lsr n) land 1 = 1
  let clear_bit n x = x land (lnot (1 lsl n))
  let set_bit n x = x lor (1 lsl n)
  let bit (x : t) (n : int) (v : bool) : t = 
    if v then set_bit n x else clear_bit n x


  let from_int (n : int) : t = n
  let to_int (x : t) : int = x

end

module Word32 = struct

  type t = Int32.t

  let eq_dec x y = x = y

  let test_bit n x = 
    Int32.logand (Int32.shift_right_logical x n) Int32.one = Int32.one
  let clear_bit n x =
    Int32.logand x (Int32.lognot (Int32.shift_left Int32.one n))
  let set_bit n x = 
    Int32.logor x (Int32.shift_left Int32.one n)
  let bit (x : t) (n : int) (v : bool) : t = 
    if v then set_bit n x else clear_bit n x

  let to_int32 (x : t) : Int32.t = x
  let from_int32 (n : Int32.t) : t = n

end

module Word48 = struct

  type t = string

  let eq_dec x y = x = y

  let test_bit n x = failwith "undefined"
(*    Int64.logand (Int64.shift_right_logical x n) Int64.one = Int64.one *)
  let clear_bit n x = failwith "undefined"
(*    Int64.logand x (Int64.lognot (Int64.shift_left Int64.one n)) *)
  let set_bit n x = failwith "undefined"
(*    Int64.logor x (Int64.shift_left Int64.one n) *)
  let bit (x : t) (n : int) (v : bool) : t = failwith "undefined"
(*    if v then set_bit n x else clear_bit n x *)

  let to_bytes (x : t) = x

  let from_bytes (s : string) : t =
    if String.length s <> 6 then
      failwith "Word64.from_bytes expects a six-byte string"
    else
      s

end


module Word64 = struct

  type t = Int64.t

  let eq_dec x y = x = y


  let test_bit n x = 
    Int64.logand (Int64.shift_right_logical x n) Int64.one = Int64.one
  let clear_bit n x =
    Int64.logand x (Int64.lognot (Int64.shift_left Int64.one n))
  let set_bit n x = 
    Int64.logor x (Int64.shift_left Int64.one n)
  let bit (x : t) (n : int) (v : bool) : t = 
    if v then set_bit n x else clear_bit n x

end
