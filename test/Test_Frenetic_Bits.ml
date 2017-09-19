open QuickCheck
open Frenetic_Bits

let arbitrary_bit32 = QuickCheck_gen.choose_int (0, 31)

let arbitrary_int_and_bit = arbitrary_pair arbitrary_int32 arbitrary_bit32
let arbitrary_bit_int_bool = arbitrary_triple arbitrary_bit32 arbitrary_int32 arbitrary_bool

let show_int_and_bit ib = show_pair show_int32 show_int ib 
let show_bit_int_bool ibb = show_triple show_int show_int32 show_bool ibb 

let prop_clear_bit (x, n) = 
  test_bit n (clear_bit n x) = false

let%test "bit n of (clear_bit n x) is zero" =
  let test = testable_fun arbitrary_int_and_bit show_int_and_bit testable_bool in
  match quickCheck test prop_clear_bit with
    | Success -> true
    | Failure _ -> failwith "No failure expected"
    | Exhausted _ -> failwith "No exhaustion expected"

let prop_set_bit (x, n) =
  test_bit n (set_bit n x)

let%test "bit n of (set_bit n x) is one" =
  let test = testable_fun arbitrary_int_and_bit show_int_and_bit testable_bool in
  match quickCheck test prop_set_bit with
    | Success -> true
    | Failure _ -> failwith "No failure expected"
    | Exhausted _ -> failwith "No exhaustion expected"

let prop_bit (n, x, b) =
  test_bit n (bit x n b) = b

let%test "bit n of (bit n x b) is b" =
  let test = testable_fun arbitrary_bit_int_bool show_bit_int_bool testable_bool in
  match quickCheck test prop_bit with
    | Success -> true
    | Failure _ -> failwith "No failure expected"
    | Exhausted _ -> failwith "No exhaustion expected"

(* Because there are no analogous set_byte32 functions, it's easier just to test these straight *)

let%test "get_byte32 3 of 0xffeeddcc is 0xff" =
  get_byte32 0xffeeddccl 3 = 0xff

let%test "get_byte32 0 of 0xffeeddcc is 0xcc" =
  get_byte32 0xffeeddccl 0 = 0xcc

let%test "get_byte32 with bit out of range generates error" =
  try
    get_byte32 0xffeeddccl 5 = max_int    
  with 
  | Invalid_argument _ -> true
  |  _ -> false



     

