type int8 = int

type int16 = int

type int48 = int64

let test_bit (n:int) (x:int32) : bool =
  Int32.logand (Int32.shift_right_logical x n) Int32.one = Int32.one

let clear_bit (n:int) (x:int32) : int32 =
  Int32.logand x (Int32.lognot (Int32.shift_left Int32.one n))

let set_bit (n:int) (x:int32) : int32 =
  Int32.logor x (Int32.shift_left Int32.one n)

let bit (x : int32) (n : int) (v : bool) : int32 =
  if v then set_bit n x else clear_bit n x
