let clear_bit (n:int) (x:int32) : int32 =
  Int32.logand x (Int32.lognot (Int32.shift_left Int32.one n))

let set_bit (n:int) (x:int32) : int32 =
  Int32.logor x (Int32.shift_left Int32.one n)

let bit (x : int32) (n : int) (v : bool) : int32 =
  if v then set_bit n x else clear_bit n x

let test_bit (n:int) (x:int32) : bool =
  Int32.logand (Int32.shift_right_logical x n) Int32.one = Int32.one

let get_byte32 (n : Int32.t) (i : int) : int = 
  let open Int32 in
  if i < 0 || i > 3 then
    raise (Invalid_argument "get_byte32 index out of range");
  to_int (logand 0xFFl (shift_right_logical n (8 * i)))

let get_byte (n:int64) (i:int) : int =
  if i < 0 || i > 7 then
    raise (Invalid_argument "Int64.get_byte index out of range");
  Int64.to_int (Int64.logand 0xFFL (Int64.shift_right_logical n (8 * i)))
