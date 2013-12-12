let test_bit n x =
  Int32.logand (Int32.shift_right_logical x n) Int32.one = Int32.one
let clear_bit n x =
  Int32.logand x (Int32.lognot (Int32.shift_left Int32.one n))
let set_bit n x =
  Int32.logor x (Int32.shift_left Int32.one n)
let bit (x : int32) (n : int) (v : bool) : int32 =
  if v then set_bit n x else clear_bit n x
