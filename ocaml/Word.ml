module Int64Ex = struct

  open Int64

  let get_byte (n : t) (i : int) : char = 
    if i < 0 or i > 5 then
      raise (Invalid_argument "Int64Ex.get_byte index out of range");
    Char.chr (to_int (logand 0xFFL (shift_right_logical n (8 * i))))
      
  let from_bytes (str : string) : t = 
    if String.length str != 8 then
      raise (Invalid_argument "Int64Ex.from_bytes expected eight-byte string");
    let n = ref 0L in
    for i = 0 to 7 do
      let b = of_int (Char.code (String.get str i)) in
      n := logor !n (shift_left b (8 * i))
    done;
    !n

end

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

  let succ (x : t) : t = x + 1

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

  let from_int (n : int) : t = Int32.of_int n

end

module Word48 = struct

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

  let to_bytes (x : t) = 
    Format.sprintf "%c%c%c%c%c%c"
      (Int64Ex.get_byte x 5) (Int64Ex.get_byte x 4) (Int64Ex.get_byte x 3)
      (Int64Ex.get_byte x 2) (Int64Ex.get_byte x 1) (Int64Ex.get_byte x 0)

  let from_bytes (str : string) : t = 
    Int64Ex.from_bytes (Format.sprintf "\x00\x00%s" str)

  let from_int64 (n : Int64.t) : t = n

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

  let from_int64 (n : Int64.t) : t = n
  let to_int64 (x : t) : Int64.t = x

  let from_int (n : int) : t = Int64.of_int n

end
